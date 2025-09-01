{- MANUALLY FORMATTED -}
module Compiler.Parse.Type exposing
  ( expression
  , variant
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- TYPE TERMS


term : P.Parser E.Type Src.Type
term =
  P.bind P.getPosition <| \start ->
  P.oneOf E.TStart
    [
      -- types with no arguments (Int, Float, etc.)
      P.bind (Var.foreignUpper E.TStart) <| \upper ->
      P.bind P.getPosition <| \end ->
      let region = A.Region start end in
      P.return <| A.At region <|
        case upper of
          Var.Unqualified name ->
            Src.TType region name []

          Var.Qualified home name ->
            Src.TTypeQual region home name []
    ,
      -- type variables
      P.bind (Var.lower E.TStart) <| \var ->
      P.addEnd start (Src.TVar var)
    ,
      -- tuples
      P.inContext E.TTuple (P.word1 0x28 {-(-} E.TStart) <|
        P.oneOf E.TTupleOpen
          [ P.bind (P.word1 0x29 {-)-} E.TTupleOpen) <| \_ ->
            P.addEnd start Src.TUnit
          , P.bind (Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentType1) <| \_ ->
            P.bind (P.specialize E.TTupleType expression) <| \(tipe, end) ->
            P.bind (Space.checkIndent end E.TTupleIndentEnd) <| \_ ->
            chompTupleEnd start tipe []
          ]
    ,
      -- records
      P.inContext E.TRecord (P.word1 0x7B {- { -} E.TStart) <|
        P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen) <| \_ ->
        P.oneOf E.TRecordOpen
          [ P.bind (P.word1 0x7D {-}-} E.TRecordEnd) <| \_ ->
            P.addEnd start (Src.TRecord [] Nothing)
          , P.bind (P.addLocation (Var.lower E.TRecordField)) <| \name ->
            P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon) <| \_ ->
            P.oneOf E.TRecordColon
              [ P.bind (P.word1 0x7C {- | -} E.TRecordColon) <| \_ ->
                P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField) <| \_ ->
                P.bind chompField <| \field ->
                P.bind (chompRecordEnd [field]) <| \fields ->
                P.addEnd start (Src.TRecord fields (Just name))
              , P.bind (P.word1 0x3A {-:-} E.TRecordColon) <| \_ ->
                P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType) <| \_ ->
                P.bind (P.specialize E.TRecordType expression) <| \(tipe, end) ->
                P.bind (Space.checkIndent end E.TRecordIndentEnd) <| \_ ->
                P.bind (chompRecordEnd [(name, tipe)]) <| \fields ->
                P.addEnd start (Src.TRecord fields Nothing)
              ]
          ]
    ]



-- TYPE EXPRESSIONS


expression : Space.Parser E.Type Src.Type
expression =
  P.bind P.getPosition <| \start ->
  P.bind
    (P.oneOf E.TStart
      [ app start
      , P.bind term <| \eterm ->
        P.bind P.getPosition <| \end ->
        P.bind (Space.chomp E.TSpace) <| \_ ->
        P.return (eterm, end)
      ]) <| \((tipe1, end1) as term1) ->
  P.oneOfWithFallback
    [ P.bind (Space.checkIndent end1 E.TIndentStart) <| \_ -> -- should never trigger
      P.bind (P.word2 0x2D 0x3E {-->-} E.TStart) <| \_ -> -- could just be another type instead
      P.bind (Space.chompAndCheckIndent E.TSpace E.TIndentStart) <| \_ ->
      P.bind expression <| \(tipe2, end2) ->
      let tipe = A.at start end2 (Src.TLambda tipe1 tipe2) in
      P.return ( tipe, end2 )
    ]
    term1



-- TYPE CONSTRUCTORS


app : A.Position -> Space.Parser E.Type Src.Type
app start =
  P.bind (Var.foreignUpper E.TStart) <| \upper ->
  P.bind P.getPosition <| \upperEnd ->
  P.bind (Space.chomp E.TSpace) <| \_ ->
  P.bind (chompArgs [] upperEnd) <| \(args, end) ->

  let region = A.Region start upperEnd in
  let tipe =
        case upper of
          Var.Unqualified name ->
            Src.TType region name args

          Var.Qualified home name ->
            Src.TTypeQual region home name args
  in
  P.return ( A.at start end tipe, end )


chompArgs : TList Src.Type -> A.Position -> Space.Parser E.Type (TList Src.Type)
chompArgs args end =
  P.oneOfWithFallback
    [ P.bind (Space.checkIndent end E.TIndentStart) <| \_ ->
      P.bind term <| \arg ->
      P.bind P.getPosition <| \newEnd ->
      P.bind (Space.chomp E.TSpace) <| \_ ->
      chompArgs (arg::args) newEnd
    ]
    (MList.reverse args, end)



-- TUPLES


chompTupleEnd : A.Position -> Src.Type -> TList Src.Type -> P.Parser E.TTuple Src.Type
chompTupleEnd start firstType revTypes =
  P.oneOf E.TTupleEnd
    [ P.bind (P.word1 0x2C {-,-} E.TTupleEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN) <| \_ ->
      P.bind (P.specialize E.TTupleType expression) <| \(tipe, end) ->
      P.bind (Space.checkIndent end E.TTupleIndentEnd) <| \_ ->
      chompTupleEnd start firstType (tipe :: revTypes)
    , P.bind (P.word1 0x29 {-)-} E.TTupleEnd) <| \_ ->
      case MList.reverse revTypes of
        [] ->
          P.return firstType

        secondType :: otherTypes ->
          P.addEnd start (Src.TTuple firstType secondType otherTypes)
    ]



-- RECORD


type alias Field = ( A.Located Name.Name, Src.Type )


chompRecordEnd : TList Field -> P.Parser E.TRecord (TList Field)
chompRecordEnd fields =
  P.oneOf E.TRecordEnd
    [ P.bind (P.word1 0x2C {-,-} E.TRecordEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField) <| \_ ->
      P.bind chompField <| \field ->
      chompRecordEnd (field :: fields)
    , P.bind (P.word1 0x7D {-}-} E.TRecordEnd) <| \_ ->
      P.return (MList.reverse fields)
    ]


chompField : P.Parser E.TRecord Field
chompField =
  P.bind (P.addLocation (Var.lower E.TRecordField)) <| \name ->
  P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon) <| \_ ->
  P.bind (P.word1 0x3A {-:-} E.TRecordColon) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType) <| \_ ->
  P.bind (P.specialize E.TRecordType expression) <| \(tipe, end) ->
  P.bind (Space.checkIndent end E.TRecordIndentEnd) <| \_ ->
  P.return (name, tipe)



-- VARIANT


variant : Space.Parser E.CustomType (A.Located Name.Name, TList Src.Type)
variant =
  P.bind (P.addLocation (Var.upper E.CT_Variant)) <| \((A.At (A.Region _ nameEnd) _) as name) ->
  P.bind (Space.chomp E.CT_Space) <| \_ ->
  P.bind (P.specialize E.CT_VariantArg (chompArgs [] nameEnd)) <| \(args, end) ->
  P.return ( (name, args), end )
