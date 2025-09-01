{- MANUALLY FORMATTED -}
module Compiler.Parse.Expression exposing
  ( expression
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- TERMS


term : P.Parser E.Expr Src.Expr
term =
  P.bind P.getPosition <| \start ->
  P.oneOf E.Start
    [ P.bind (variable start) (accessible start)
    , string start
    , number start
    , Shader.shader start
    , list start
    , P.bind (record start) (accessible start)
    , P.bind (tuple start) (accessible start)
    , accessor start
    , character start
    ]


string : A.Position -> P.Parser E.Expr Src.Expr
string start =
  P.bind (String.string E.Start E.CString) <| \str ->
  P.addEnd start (Src.Str str)


character : A.Position -> P.Parser E.Expr Src.Expr
character start =
  P.bind (String.character E.Start E.CChar) <| \chr ->
  P.addEnd start (Src.Chr chr)


number : A.Position -> P.Parser E.Expr Src.Expr
number start =
  P.bind (Number.number E.Start E.Number) <| \nmbr ->
  P.addEnd start <|
    case nmbr of
      Number.CInt int -> Src.CInt int
      Number.CFloat float -> Src.CFloat float


accessor : A.Position -> P.Parser E.Expr Src.Expr
accessor start =
  P.bind (P.word1 0x2E {-.-} E.Dot) <| \_ ->
  P.bind (Var.lower E.Access) <| \field ->
  P.addEnd start (Src.Accessor field)


variable : A.Position -> P.Parser E.Expr Src.Expr
variable start =
  P.bind (Var.foreignAlpha E.Start) <| \var ->
  P.addEnd start var


accessible : A.Position -> Src.Expr -> P.Parser E.Expr Src.Expr
accessible start expr =
  P.oneOfWithFallback
    [ P.bind (P.word1 0x2E {-.-} E.Dot) <| \_ ->
      P.bind P.getPosition <| \pos ->
      P.bind (Var.lower E.Access) <| \field ->
      P.bind P.getPosition <| \end ->
      accessible start <|
        A.at start end (Src.Access expr (A.at pos end field))
    ]
    expr



-- LISTS


list : A.Position -> P.Parser E.Expr Src.Expr
list start =
  P.inContext E.CList (P.word1 0x5B {-[-} E.Start) <|
    P.bind (Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen) <| \_ ->
    P.oneOf E.ListOpen
      [ P.bind (P.specialize E.ListExpr expression) <| \(entry, end) ->
        P.bind (Space.checkIndent end E.ListIndentEnd) <| \_ ->
        P.loop (chompListEnd start) [entry]
      , P.bind (P.word1 0x5D {-]-} E.ListOpen) <| \_ ->
        P.addEnd start (Src.CList [])
      ]


chompListEnd : A.Position -> TList Src.Expr -> P.Parser E.TList (P.Step (TList Src.Expr) Src.Expr)
chompListEnd start entries =
  P.oneOf E.ListEnd
    [ P.bind (P.word1 0x2C {-,-} E.ListEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr) <| \_ ->
      P.bind (P.specialize E.ListExpr expression) <| \(entry, end) ->
      P.bind (Space.checkIndent end E.ListIndentEnd) <| \_ ->
      P.return (P.Loop (entry::entries))
    , P.bind (P.word1 0x5D {-]-} E.ListEnd) <| \_ ->
      P.fmap P.Done (P.addEnd start (Src.CList (MList.reverse entries)))
    ]



-- TUPLES


tuple : A.Position -> P.Parser E.Expr Src.Expr
tuple ((A.Position row col) as start) =
  P.inContext E.Tuple (P.word1 0x28 {-(-} E.Start) <|
    P.bind P.getPosition <| \before ->
    P.bind (Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1) <| \_ ->
    P.bind P.getPosition <| \after ->
    if before /= after
      then
        P.bind (P.specialize E.TupleExpr expression) <| \(entry, end) ->
        P.bind (Space.checkIndent end E.TupleIndentEnd) <| \_ ->
        chompTupleEnd start entry []
      else
        P.oneOf E.TupleIndentExpr1
          [
            P.bind (Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved) <| \op ->
            if op == "-"
              then
                P.oneOf E.TupleOperatorClose
                  [
                    P.bind (P.word1 0x29 {-)-} E.TupleOperatorClose) <| \_ ->
                    P.addEnd start (Src.Op op)
                  ,
                    P.bind
                      (P.specialize E.TupleExpr <|
                        P.bind term <| \((A.At (A.Region _ end) _) as negatedExpr) ->
                        P.bind (Space.chomp E.Space) <| \_ ->
                        let exprStart = A.Position row (col + 2) in
                        let expr = A.at exprStart end (Src.Negate negatedExpr) in
                        chompExprEnd exprStart (State [] expr [] end)) <| \(entry, end) ->
                    P.bind (Space.checkIndent end E.TupleIndentEnd) <| \_ ->
                    chompTupleEnd start entry []
                  ]
              else
                P.bind (P.word1 0x29 {-)-} E.TupleOperatorClose) <| \_ ->
                P.addEnd start (Src.Op op)
          ,
            P.bind (P.word1 0x29 {-)-} E.TupleIndentExpr1) <| \_ ->
            P.addEnd start Src.Unit
          ,
            P.bind (P.specialize E.TupleExpr expression) <| \(entry, end) ->
            P.bind (Space.checkIndent end E.TupleIndentEnd) <| \_ ->
            chompTupleEnd start entry []
          ]


chompTupleEnd : A.Position -> Src.Expr -> TList Src.Expr -> P.Parser E.Tuple Src.Expr
chompTupleEnd start firstExpr revExprs =
  P.oneOf E.TupleEnd
    [ P.bind (P.word1 0x2C {-,-} E.TupleEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN) <| \_ ->
      P.bind (P.specialize E.TupleExpr expression) <| \(entry, end) ->
      P.bind (Space.checkIndent end E.TupleIndentEnd) <| \_ ->
      chompTupleEnd start firstExpr (entry :: revExprs)
    , P.bind (P.word1 0x29 {-)-} E.TupleEnd) <| \_ ->
      case MList.reverse revExprs of
        [] ->
          P.return firstExpr

        secondExpr :: otherExprs ->
          P.addEnd start (Src.Tuple firstExpr secondExpr otherExprs)
    ]



-- RECORDS


record : A.Position -> P.Parser E.Expr Src.Expr
record start =
  P.inContext E.Record (P.word1 0x7B {- { -} E.Start) <|
    P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen) <| \_ ->
    P.oneOf E.RecordOpen
      [ P.bind (P.word1 0x7D {-}-} E.RecordOpen) <| \_ ->
        P.addEnd start (Src.Record [])
      , P.bind (P.addLocation (Var.lower E.RecordField)) <| \starter ->
        P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals) <| \_ ->
        P.oneOf E.RecordEquals
          [ P.bind (P.word1 0x7C {- | -} E.RecordEquals) <| \_ ->
            P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField) <| \_ ->
            P.bind chompField <| \firstField ->
            P.bind (chompFields [firstField]) <| \fields ->
            P.addEnd start (Src.Update starter fields)
          , P.bind (P.word1 0x3D {-=-} E.RecordEquals) <| \_ ->
            P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr) <| \_ ->
            P.bind (P.specialize E.RecordExpr expression) <| \(value, end) ->
            P.bind (Space.checkIndent end E.RecordIndentEnd) <| \_ ->
            P.bind (chompFields [(starter, value)]) <| \fields ->
            P.addEnd start (Src.Record fields)
          ]
      ]


type alias Field = ( A.Located Name.Name, Src.Expr )


chompFields : TList Field -> P.Parser E.Record (TList Field)
chompFields fields =
  P.oneOf E.RecordEnd
    [ P.bind (P.word1 0x2C {-,-} E.RecordEnd) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField) <| \_ ->
      P.bind chompField <| \f ->
      chompFields (f :: fields)
    , P.bind (P.word1 0x7D {-}-} E.RecordEnd) <| \_ ->
      P.return (MList.reverse fields)
    ]


chompField : P.Parser E.Record Field
chompField =
  P.bind (P.addLocation (Var.lower E.RecordField)) <| \key ->
  P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals) <| \_ ->
  P.bind (P.word1 0x3D {-=-} E.RecordEquals) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr) <| \_ ->
  P.bind (P.specialize E.RecordExpr expression) <| \(value, end) ->
  P.bind (Space.checkIndent end E.RecordIndentEnd) <| \_ ->
  P.return (key, value)



-- EXPRESSIONS


expression : Space.Parser E.Expr Src.Expr
expression =
  P.bind P.getPosition <| \start ->
  P.oneOf E.Start
    [ let_ start
    , if_ start
    , case_ start
    , function start
    , P.bind (possiblyNegativeTerm start) <| \expr ->
      P.bind P.getPosition <| \end ->
      P.bind (Space.chomp E.Space) <| \_ ->
      chompExprEnd start (State [] expr [] end)
    ]


type State =
  State
    {- ops  -} (TList (Src.Expr, A.Located Name.Name))
    {- expr -} Src.Expr
    {- args -} (TList Src.Expr)
    {- end  -} A.Position


chompExprEnd : A.Position -> State -> Space.Parser E.Expr Src.Expr
chompExprEnd start (State ops expr args end) =
  P.oneOfWithFallback
    [ -- argument
      P.bind (Space.checkIndent end E.Start) <| \_ ->
      P.bind term <| \arg ->
      P.bind P.getPosition <| \newEnd ->
      P.bind (Space.chomp E.Space) <| \_ ->
      chompExprEnd start (State ops expr (arg::args) newEnd)

    , -- operator
      P.bind (Space.checkIndent end E.Start) <| \_ ->
      P.bind (P.addLocation (Symbol.operator E.Start E.OperatorReserved)) <| \((A.At (A.Region opStart opEnd) opName) as op) ->
      P.bind (Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)) <| \_ ->
      P.bind P.getPosition <| \newStart ->
      if "-" == opName && end /= opStart && opEnd == newStart
        then
          -- negative terms
          P.bind term <| \negatedExpr ->
          P.bind P.getPosition <| \newEnd ->
          P.bind (Space.chomp E.Space) <| \_ ->
          let arg = A.at opStart newEnd (Src.Negate negatedExpr) in
          chompExprEnd start (State ops expr (arg::args) newEnd)
        else
          let err = E.OperatorRight opName in
          P.oneOf err
            [ -- term
              P.bind (possiblyNegativeTerm newStart) <| \newExpr ->
              P.bind P.getPosition <| \newEnd ->
              P.bind (Space.chomp E.Space) <| \_ ->
              let newOps = (toCall expr args, op) :: ops in
              chompExprEnd start (State newOps newExpr [] newEnd)

            , -- final term
              P.bind
                (P.oneOf err
                  [ let_ newStart
                  , case_ newStart
                  , if_ newStart
                  , function newStart
                  ]) <| \(newLast, newEnd) ->
              let newOps = (toCall expr args, op) :: ops in
              let finalExpr = Src.Binops (MList.reverse newOps) newLast in
              P.return ( A.at start newEnd finalExpr, newEnd )
            ]

    ]
    -- done
    (
      case ops of
        [] ->
          ( toCall expr args
          , end
          )

        _ ->
          ( A.at start end (Src.Binops (MList.reverse ops) (toCall expr args))
          , end
          )
    )


possiblyNegativeTerm : A.Position -> P.Parser E.Expr Src.Expr
possiblyNegativeTerm start =
  P.oneOf E.Start
    [ P.bind (P.word1 0x2D {---} E.Start) <| \_ ->
      P.bind term <| \expr ->
      P.addEnd start (Src.Negate expr)
    , term
    ]


toCall : Src.Expr -> TList Src.Expr -> Src.Expr
toCall func revArgs =
  case revArgs of
    [] ->
      func

    lastArg :: _ ->
      A.merge func lastArg (Src.Call func (MList.reverse revArgs))



-- IF EXPRESSION


if_ : A.Position -> Space.Parser E.Expr Src.Expr
if_ start =
  P.inContext E.If (Keyword.if_ E.Start) <|
    chompIfEnd start []


chompIfEnd : A.Position -> TList (Src.Expr, Src.Expr) -> Space.Parser E.If Src.Expr
chompIfEnd start branches =
  P.bind (Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition) <| \_ ->
  P.bind (P.specialize E.IfCondition expression) <| \(condition, condEnd) ->
  P.bind (Space.checkIndent condEnd E.IfIndentThen) <| \_ ->
  P.bind (Keyword.then_ E.IfThen) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch) <| \_ ->
  P.bind (P.specialize E.IfThenBranch expression) <| \(thenBranch, thenEnd) ->
  P.bind (Space.checkIndent thenEnd E.IfIndentElse) <| \_ ->
  P.bind (Keyword.else_ E.IfElse) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch) <| \_ ->
  let newBranches = (condition, thenBranch) :: branches in
  P.oneOf E.IfElseBranchStart
    [
      P.bind (Keyword.if_ E.IfElseBranchStart) <| \_ ->
      chompIfEnd start newBranches
    ,
      P.bind (P.specialize E.IfElseBranch expression) <| \(elseBranch, elseEnd) ->
      let ifExpr = Src.If (MList.reverse newBranches) elseBranch in
      P.return ( A.at start elseEnd ifExpr, elseEnd )
    ]



-- LAMBDA EXPRESSION


function : A.Position -> Space.Parser E.Expr Src.Expr
function start =
  P.inContext E.Func (P.word1 0x5C {-\-} E.Start) <|
    P.bind (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg) <| \_ ->
    P.bind (P.specialize E.FuncArg Pattern.term) <| \arg ->
    P.bind (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow) <| \_ ->
    P.bind (chompArgs [arg]) <| \revArgs ->
    P.bind (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody) <| \_ ->
    P.bind (P.specialize E.FuncBody expression) <| \(body, end) ->
    let funcExpr = Src.Lambda (MList.reverse revArgs) body in
    P.return (A.at start end funcExpr, end)


chompArgs : TList Src.Pattern -> P.Parser E.Func (TList Src.Pattern)
chompArgs revArgs =
  P.oneOf E.FuncArrow
    [ P.bind (P.specialize E.FuncArg Pattern.term) <| \arg ->
      P.bind (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow) <| \_ ->
      chompArgs (arg::revArgs)
    , P.bind (P.word2 0x2D 0x3E {-->-} E.FuncArrow) <| \_ ->
      P.return revArgs
    ]



-- CASE EXPRESSIONS


case_ : A.Position -> Space.Parser E.Expr Src.Expr
case_ start =
  P.inContext E.Case (Keyword.case_ E.Start) <|
    P.bind (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr) <| \_ ->
    P.bind (P.specialize E.CaseExpr expression) <| \(expr, exprEnd) ->
    P.bind (Space.checkIndent exprEnd E.CaseIndentOf) <| \_ ->
    P.bind (Keyword.of_ E.CaseOf) <| \_ ->
    P.bind (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern) <| \_ ->
    P.withIndent <|
      P.bind chompBranch <| \(firstBranch, firstEnd) ->
      P.bind (chompCaseEnd [firstBranch] firstEnd) <| \(branches, end) ->
      P.return
        ( A.at start end (Src.Case expr branches)
        , end
        )


chompBranch : Space.Parser E.Case (Src.Pattern, Src.Expr)
chompBranch =
  P.bind (P.specialize E.CasePattern Pattern.expression) <| \(pattern, patternEnd) ->
  P.bind (Space.checkIndent patternEnd E.CaseIndentArrow) <| \_ ->
  P.bind (P.word2 0x2D 0x3E {-->-} E.CaseArrow) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch) <| \_ ->
  P.bind (P.specialize E.CaseBranch expression) <| \(branchExpr, end) ->
  P.return ( (pattern, branchExpr), end )


chompCaseEnd : TList (Src.Pattern, Src.Expr) -> A.Position -> Space.Parser E.Case (TList (Src.Pattern, Src.Expr))
chompCaseEnd branches end =
  P.oneOfWithFallback
    [ P.bind (Space.checkAligned E.CasePatternAlignment) <| \_ ->
      P.bind chompBranch <| \(branch, newEnd) ->
      chompCaseEnd (branch::branches) newEnd
    ]
    (MList.reverse branches, end)



-- LET EXPRESSION


let_ : A.Position -> Space.Parser E.Expr Src.Expr
let_ start =
  P.inContext E.Let (Keyword.let_ E.Start) <|
    P.bind
      (P.withBacksetIndent 3 <|
        P.bind (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef) <| \_ ->
        P.withIndent <|
          P.bind chompLetDef <| \(def, end) ->
          chompLetDefs [def] end) <| \(defs, defsEnd) ->

    P.bind (Space.checkIndent defsEnd E.LetIndentIn) <| \_ ->
    P.bind (Keyword.in_ E.LetIn) <| \_ ->
    P.bind (Space.chompAndCheckIndent E.LetSpace E.LetIndentBody) <| \_ ->
    P.bind (P.specialize E.LetBody expression) <| \(body, end) ->
    P.return
      ( A.at start end (Src.Let defs body)
      , end
      )


chompLetDefs : TList (A.Located Src.Def) -> A.Position -> Space.Parser E.Let (TList (A.Located Src.Def))
chompLetDefs revDefs end =
  P.oneOfWithFallback
    [ P.bind (Space.checkAligned (\_ -> E.LetDefAlignment)) <| \_ ->
      P.bind chompLetDef <| \(def, newEnd) ->
      chompLetDefs (def::revDefs) newEnd
    ]
    (MList.reverse revDefs, end)



-- LET DEFINITIONS


chompLetDef : Space.Parser E.Let (A.Located Src.Def)
chompLetDef =
  P.oneOf E.LetDefName
    [ definition
    , destructure
    ]



-- DEFINITION


definition : Space.Parser E.Let (A.Located Src.Def)
definition =
  P.bind (P.addLocation (Var.lower E.LetDefName)) <| \((A.At (A.Region start _) name) as aname) ->
  P.specialize (E.LetDef name) <|
    P.bind (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <| \_ ->
    P.oneOf E.DefEquals
      [
        P.bind (P.word1 0x3A {-:-} E.DefEquals) <| \_ ->
        P.bind (Space.chompAndCheckIndent E.DefSpace E.DefIndentType) <| \_ ->
        P.bind (P.specialize E.DefType Type.expression) <| \(tipe, _) ->
        P.bind (Space.checkAligned E.DefAlignment) <| \_ ->
        P.bind (chompMatchingName name) <| \defName ->
        P.bind (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <| \_ ->
        chompDefArgsAndBody start defName (Just tipe) []
      ,
        chompDefArgsAndBody start aname Nothing []
      ]


chompDefArgsAndBody : A.Position -> A.Located Name.Name -> Maybe Src.Type -> TList Src.Pattern -> Space.Parser E.Def (A.Located Src.Def)
chompDefArgsAndBody start name tipe revArgs =
  P.oneOf E.DefEquals
    [ P.bind (P.specialize E.DefArg Pattern.term) <| \arg ->
      P.bind (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals) <| \_ ->
      chompDefArgsAndBody start name tipe (arg :: revArgs)
    , P.bind (P.word1 0x3D {-=-} E.DefEquals) <| \_ ->
      P.bind (Space.chompAndCheckIndent E.DefSpace E.DefIndentBody) <| \_ ->
      P.bind (P.specialize E.DefBody expression) <| \(body, end) ->
      P.return
        ( A.at start end (Src.Define name (MList.reverse revArgs) body tipe)
        , end
        )
    ]


chompMatchingName : Name.Name -> P.Parser E.Def (A.Located Name.Name)
chompMatchingName expectedName =
  let
    (P.Parser parserL) = Var.lower E.DefNameRepeat
  in
  P.Parser <| \((P.State _ _ _ _ sr sc) as state) ->
    case parserL state of
      P.Cok name ((P.State _ _ _ _ er ec) as newState) ->
        if expectedName == name
        then P.Cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else P.Cerr sr sc (E.DefNameMatch name)
      P.Eok name ((P.State _ _ _ _ er ec) as newState) ->
        if expectedName == name
        then P.Eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else P.Eerr sr sc (E.DefNameMatch name)
      P.Cerr r c t -> P.Cerr r c t
      P.Eerr r c t -> P.Eerr r c t



-- DESTRUCTURE


destructure : Space.Parser E.Let (A.Located Src.Def)
destructure =
  P.specialize E.LetDestruct <|
  P.bind P.getPosition <| \start ->
  P.bind (P.specialize E.DestructPattern Pattern.term) <| \pattern ->
  P.bind (Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals) <| \_ ->
  P.bind (P.word1 0x3D {-=-} E.DestructEquals) <| \_ ->
  P.bind (Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody) <| \_ ->
  P.bind (P.specialize E.DestructBody expression) <| \(expr, end) ->
  P.return ( A.at start end (Src.Destruct pattern expr), end )
