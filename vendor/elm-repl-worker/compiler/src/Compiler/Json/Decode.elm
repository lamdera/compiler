{- MANUALLY FORMATTED -}
module Compiler.Json.Decode exposing
  ( fromByteString
  , Decoder, fmap, pure, andMap, return, bind, andThen
  , string
  , customString
  --, bool
  --, int
  , list
  , nonEmptyList
  --, pair
  ----
  , KeyDecoder(..)
  , dict
  , pairs
  , field
  ----
  , oneOf
  , failure
  , mapError
  --
  , Error(..)
  , Problem(..)
  , DecodeExpectation(..)
  , ParseError(..)
  , StringProblem(..)
  )


import Compiler.Data.NonEmptyList as NE
import Compiler.Json.String as Json
import Compiler.Parse.Keyword as K
import Compiler.Parse.Primitives as P exposing (Row, Col)
import Compiler.Reporting.Annotation as A
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Class.Functor as Functor
import Extra.Class.Applicative as Applicative
import Extra.Class.Monad as Monad



-- RUNNERS


fromByteString : Decoder x a -> String -> Either (Error x) a
fromByteString (Decoder decode) src =
  case P.fromByteString pFile BadEnd src of
    Right ast ->
      case decode ast of
        Dok a -> Right a
        Derr p -> Left (DecodeProblem src p)

    Left problem ->
      Left (ParseProblem src problem)



-- DECODERS


type Decoder x a =
  Decoder (AST -> DStep x a)


type DStep x a
  = Dok a
  | Derr (Problem x)



-- ERRORS


type Error x
  = DecodeProblem String (Problem x)
  | ParseProblem String ParseError



-- DECODE PROBLEMS


type Problem x
  = Field String (Problem x)
  | Index Int (Problem x)
  | OneOf (Problem x) (TList (Problem x))
  | Failure A.Region x
  | Expecting A.Region DecodeExpectation


type DecodeExpectation
  = TObject
  | TArray
  | TString
  | TObjectWith String



-- INSTANCES


fmap : Functor.Fmap a (Decoder x a) b (Decoder x b)
fmap func (Decoder decodeA) =
  Decoder <| \ast ->
    case decodeA ast of
      Dok a -> Dok (func a)
      Derr p -> Derr p


pure : Applicative.Pure a (Decoder x a)
pure = return

andMap : Applicative.AndMap (Decoder x a) (Decoder x (a -> b)) (Decoder x b)
andMap (Decoder decodeArg) (Decoder decodeFunc) =
  Decoder <| \ast ->
    case decodeFunc ast of
      Dok func ->
        case decodeArg ast of
          Dok arg -> Dok (func arg)
          Derr p -> Derr p
      Derr p -> Derr p


return : Monad.Return a (Decoder x a)
return a =
  Decoder <| \_ ->
    Dok a

bind : Monad.Bind a (Decoder x a) (Decoder x b)
bind (Decoder decodeA) callback =
  Decoder <| \ast ->
    case decodeA ast of
      Dok a ->
        case callback a of
          Decoder decodeB -> decodeB ast
      Derr p -> Derr p

andThen : Monad.AndThen a (Decoder x a) (Decoder x b)
andThen = Monad.andThen bind



-- STRINGS


string : Decoder x Json.TString
string =
  Decoder <| \(A.At region ast) ->
    case ast of
      String snippet ->
        Dok (Json.fromSnippet snippet)

      _ ->
        Derr (Expecting region TString)


customString : P.Parser x a -> (Row -> Col -> x) -> Decoder x a
customString parser toBadEnd =
  Decoder <| \(A.At region ast) ->
    case ast of
      String snippet ->
        case P.fromSnippet parser toBadEnd snippet of
          Right a -> Dok a
          Left  x -> Derr (Failure region x)

      _ ->
        Derr (Expecting region TString)



-- LISTS


list : Decoder x a -> Decoder x (TList a)
list decoder =
  Decoder <| \(A.At region ast) ->
    case ast of
      Array asts ->
        listHelp decoder 0 asts []

      _ ->
        Derr (Expecting region TArray)


listHelp : Decoder x a -> Int -> TList AST -> TList a -> DStep x (TList a)
listHelp ((Decoder decodeA) as decoder) i asts revs =
  case asts of
    [] ->
      Dok (MList.reverse revs)

    ast::asts_ ->
      case decodeA ast of
        Dok value -> listHelp decoder (i+1) asts_ (value::revs)
        Derr prob -> Derr (Index i prob)



-- NON-EMPTY LISTS


nonEmptyList : Decoder x a -> x -> Decoder x (NE.TList a)
nonEmptyList decoder x =
  bind (list decoder) <| \values ->
  case values of
    v::vs -> return (NE.CList v vs)
    []    -> failure x



-- OBJECTS


type KeyDecoder x comparable =
  KeyDecoder (P.Parser x comparable) (Row -> Col -> x)


dict : KeyDecoder x comparable -> Decoder x a -> Decoder x (Map.Map comparable a)
dict keyDecoder valueDecoder =
  fmap Map.fromList <| pairs keyDecoder valueDecoder


pairs : KeyDecoder x comparable -> Decoder x a -> Decoder x (TList (comparable, a))
pairs keyDecoder valueDecoder =
  Decoder <| \(A.At region ast) ->
    case ast of
      Object kvs ->
        pairsHelp keyDecoder valueDecoder kvs []

      _ ->
        Derr (Expecting region TObject)


pairsHelp : KeyDecoder x comparable -> Decoder x a -> TList (P.Snippet, AST) -> TList (comparable, a) -> DStep x (TList (comparable, a))
pairsHelp ((KeyDecoder keyParser toBadEnd) as keyDecoder) ((Decoder decodeA) as valueDecoder) kvs revs =
  case kvs of
    [] ->
      Dok (MList.reverse revs)

    (snippet, ast) :: kvs_ ->
      case P.fromSnippet keyParser toBadEnd snippet of
        Left x ->
          Derr (Failure (snippetToRegion snippet) x)

        Right key ->
          case decodeA ast of
            Dok value -> pairsHelp keyDecoder valueDecoder kvs_ ((key,value)::revs)
            Derr prob ->
              let (P.Snippet fptr off len _ _) = snippet in
              Derr (Field (String.slice off (off + len) fptr) prob)


snippetToRegion : P.Snippet -> A.Region
snippetToRegion (P.Snippet _ _ len row col) =
  A.Region (A.Position row col) (A.Position row (col + len))



-- FIELDS


field : String -> Decoder x a -> Decoder x a
field key (Decoder decodeA) =
  Decoder <| \(A.At region ast) ->
    case ast of
      Object kvs ->
        case findField key kvs of
          Just value ->
            case decodeA value of
              --Dok a -> Dok a
              Derr prob ->
                Derr (Field key prob)
              x -> x

          Nothing ->
            Derr (Expecting region (TObjectWith key))

      _ ->
        Derr (Expecting region TObject)


findField : String -> TList (P.Snippet, AST) -> Maybe AST
findField key pairs_ =
  case pairs_ of
    [] ->
      Nothing

    (P.Snippet src off len _ _, value) :: remainingPairs ->
      if key == String.slice off (off + len) src
      then Just value
      else findField key remainingPairs



-- ONE OF


oneOf : TList (Decoder x a) -> Decoder x a
oneOf decoders =
  Decoder <| \ast ->
    case decoders of
      Decoder decodeA :: decoders_ ->
        case decodeA ast of
          --Dok a -> Dok a
          Derr e ->
            oneOfHelp ast decoders_ e []
          x -> x

      [] ->
        Debug.todo "Ran into (Json.Decode.oneOf [])"


oneOfHelp : AST -> TList (Decoder x a) -> Problem x -> TList (Problem x) -> DStep x a
oneOfHelp ast decoders p ps =
  case decoders of
    Decoder decodeA :: decoders_ ->
      case decodeA ast of
        --Dok a -> Dok a
        Derr p_ ->
          oneOfHelp ast decoders_ p_ (p::ps)
        x -> x

    [] ->
      Derr (oneOfError [] p ps)


oneOfError : TList (Problem x) -> Problem x -> TList (Problem x) -> Problem x
oneOfError problems prob ps =
  case ps of
    [] ->
      OneOf prob problems

    p::ps_ ->
      oneOfError (prob::problems) p ps_



-- FAILURE


failure : x -> Decoder x a
failure x =
  Decoder <| \(A.At region _) ->
    Derr (Failure region x)



-- ERRORS


mapError : (x -> y) -> Decoder x a -> Decoder y a
mapError func (Decoder decodeA) =
  Decoder <| \ast ->
    case decodeA ast of
      Dok a -> Dok a
      Derr prob -> Derr (mapErrorHelp func prob)


mapErrorHelp : (x -> y) -> Problem x -> Problem y
mapErrorHelp func problem =
  case problem of
    Field k p     -> Field k (mapErrorHelp func p)
    Index i p     -> Index i (mapErrorHelp func p)
    OneOf p ps    -> OneOf (mapErrorHelp func p) (MList.map (mapErrorHelp func) ps)
    Failure r x   -> Failure r (func x)
    Expecting r e -> Expecting r e



-- AST


type alias AST =
  A.Located AST_


type AST_
  = Array (TList AST)
  | Object (TList (P.Snippet, AST))
  | String P.Snippet
  | Int
  | TRUE
  | FALSE
  | NULL



-- PARSE


type alias Parser a =
  P.Parser ParseError a


type ParseError
  = Start Row Col
  | ObjectField Row Col
  | ObjectColon Row Col
  | ObjectEnd Row Col
  | ArrayEnd Row Col
  | StringProblem StringProblem Row Col
  | NoLeadingZeros Row Col
  | NoFloats Row Col
  | BadEnd Row Col

--  PIndex Int ParseError Row Col
--  PField Json.String ParseError Row Col


type StringProblem
  = BadStringEnd
  | BadStringControlChar
  | BadStringEscapeChar
  | BadStringEscapeHex



-- PARSE AST


pFile : Parser AST
pFile =
  P.bind spaces <| \_ ->
  P.bind pValue <| \value ->
  P.bind spaces <| \_ ->
  P.return value


pValue : Parser AST
pValue =
  P.addLocation <|
  P.oneOf Start
    [ P.fmap String <| pString Start
    , pObject
    , pArray
    , pInt
    , P.bind (K.k4 0x74 0x72 0x75 0x65      Start) <| \_ -> P.return TRUE
    , P.bind (K.k5 0x66 0x61 0x6C 0x73 0x65 Start) <| \_ -> P.return FALSE
    , P.bind (K.k4 0x6E 0x75 0x6C 0x6C      Start) <| \_ -> P.return NULL
    ]



-- OBJECT


pObject : Parser AST_
pObject =
  P.bind (P.word1 0x7B {- { -} Start) <| \_ ->
  P.bind spaces <| \_ ->
  P.oneOf ObjectField
    [ P.bind pField <| \entry ->
      P.bind spaces <| \_ ->
      P.loop pObjectHelp [entry]
    , P.bind (P.word1 0x7D {-}-} ObjectEnd) <| \_ ->
      P.return (Object [])
    ]


pObjectHelp : TList (P.Snippet, AST) -> Parser (P.Step (TList (P.Snippet, AST)) AST_)
pObjectHelp revEntries =
  P.oneOf ObjectEnd
    [
      P.bind (P.word1 0x2C {-,-} ObjectEnd) <| \_ ->
      P.bind spaces <| \_ ->
      P.bind pField <| \entry ->
      P.bind spaces <| \_ ->
      P.return (P.Loop (entry::revEntries))
    ,
      P.bind (P.word1 0x7D {-}-} ObjectEnd) <| \_ ->
      P.return (P.Done (Object (MList.reverse revEntries)))
    ]


pField : Parser (P.Snippet, AST)
pField =
  P.bind (pString ObjectField) <| \key ->
  P.bind spaces <| \_ ->
  P.bind (P.word1 0x3A {-:-} ObjectColon) <| \_ ->
  P.bind spaces <| \_ ->
  P.bind pValue <| \value ->
  P.return (key, value)



-- ARRAY


pArray : Parser AST_
pArray =
  P.bind (P.word1 0x5B {-[-} Start) <| \_ ->
  P.bind spaces <| \_ ->
  P.oneOf Start
    [ P.bind pValue <| \entry ->
      P.bind spaces <| \_ ->
      P.loop pArrayHelp (1, [entry])
    , P.bind (P.word1 0x5D {-]-} ArrayEnd) <| \_ ->
      P.return (Array [])
    ]


pArrayHelp : (Int, TList AST) -> Parser (P.Step (Int, TList AST) AST_)
pArrayHelp (len, revEntries) =
  P.oneOf ArrayEnd
    [
      P.bind (P.word1 0x2C {-,-} ArrayEnd) <| \_ ->
      P.bind spaces <| \_ ->
      P.bind pValue <| \entry ->
      P.bind spaces <| \_ ->
      P.return (P.Loop (len + 1, entry::revEntries))
    ,
      P.bind (P.word1 0x5D {-]-} ArrayEnd) <| \_ ->
      P.return (P.Done (Array (MList.reverse revEntries)))
    ]



-- STRING


pString : (Row -> Col -> ParseError) -> Parser P.Snippet
pString start =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos < end && P.unsafeIndex src pos == 0x22 {-"-} then

      let
        pos1 = pos + 1
        col1 = col + 1

        ((status, newPos), (newRow, newCol)) =
          pStringHelp src pos1 end row col1
      in
      case status of
        GoodString ->
          let
            off = pos1
            len = newPos - pos1 - 1
            snp = P.Snippet src off len row col1
            newState = P.State src newPos end indent newRow newCol
          in
          P.Cok snp newState

        BadString problem ->
          P.Cerr newRow newCol (StringProblem problem)

    else
      P.Eerr row col start


type StringStatus
  = GoodString
  | BadString StringProblem


pStringHelp : String -> Int -> Int -> Row -> Col -> ((StringStatus, Int), (Row, Col))
pStringHelp src pos end row col =
  if pos >= end then
    ((BadString BadStringEnd, pos), (row, col))

  else
    case P.unsafeIndex src pos of
      0x22 {-"-} ->
        ((GoodString, pos + 1), (row, col + 1))

      0x0A {-\n-} ->
        ((BadString BadStringEnd, pos), (row, col))

      0x5C {-\-} ->
        let pos1 = pos + 1 in
        if pos1 >= end then
          ((BadString BadStringEnd, pos1), (row + 1, col))
        else
          case P.unsafeIndex src pos1 of
            0x22 {-"-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x5C {-\-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x2F {-/-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x62 {-b-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x66 {-f-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x6E {-n-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x72 {-r-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x74 {-t-} -> pStringHelp src (pos + 2) end row (col + 2)
            0x75 {-u-} ->
              let pos6 = pos + 6 in
              if pos6 <= end
                && isHex (P.unsafeIndex src (pos + 2))
                && isHex (P.unsafeIndex src (pos + 3))
                && isHex (P.unsafeIndex src (pos + 4))
                && isHex (P.unsafeIndex src (pos + 5))
              then
                pStringHelp src pos6 end row (col + 6)
              else
                ((BadString BadStringEscapeHex, pos), (row, col))

            _ ->
              ((BadString BadStringEscapeChar, pos), (row, col))

      word ->
        if word < 0x20 then
          ((BadString BadStringControlChar, pos), (row, col))
        else
          let newPos = pos + (P.getCharWidth word) in
          pStringHelp src newPos end row (col + 1)


isHex : Int -> Bool
isHex word =
     0x30 {-0-} <= word && word <= 0x39 {-9-}
  || 0x61 {-a-} <= word && word <= 0x66 {-f-}
  || 0x41 {-A-} <= word && word <= 0x46 {-F-}



-- SPACES


spaces : Parser ()
spaces =
  P.Parser <| \((P.State src pos end indent row col) as state) ->
    let
      (newPos, newRow, newCol) =
        eatSpaces src pos end row col
    in
    if pos == newPos then
      P.Eok () state
    else
      let
        newState =
          P.State src newPos end indent newRow newCol
      in
      P.Cok () newState


eatSpaces : String -> Int -> Int -> Row -> Col -> (Int, Row, Col)
eatSpaces src pos end row col =
  if pos >= end then
    (pos, row, col)

  else
    case P.unsafeIndex src pos of
      0x20 {-  -} -> eatSpaces src (pos + 1) end row (col + 1)
      0x09 {-\t-} -> eatSpaces src (pos + 1) end row (col + 1)
      0x0A {-\n-} -> eatSpaces src (pos + 1) end (row + 1) 1
      0x0D {-\r-} -> eatSpaces src (pos + 1) end row col
      _ ->
        (pos, row, col)



-- INTS


pInt : Parser AST_
pInt =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos >= end then
      P.Eerr row col Start

    else
      let word = P.unsafeIndex src pos in
      if not (isDecimalDigit word) then
        P.Eerr row col Start

      else if word == 0x30 {-0-} then

        let
          pos1 = pos + 1
          newState = P.State src pos1 end indent row (col + 1)
        in
        if pos1 < end then
          let word1 = P.unsafeIndex src pos1 in
          if isDecimalDigit word1 then
            P.Cerr row (col + 1) NoLeadingZeros
          else if word1 == 0x2E {-.-} then
            P.Cerr row (col + 1) NoFloats
          else
            P.Cok Int newState
        else
          P.Cok Int newState

      else
        let
          (status, _, newPos) =
            chompInt src (pos + 1) end (word - 0x30 {-0-})

          len = newPos - pos
        in
        case status of
          GoodInt ->
            let
              newState =
                P.State src newPos end indent row (col + len)
            in
            P.Cok Int newState

          BadIntEnd ->
            P.Cerr row (col + len) NoFloats


type IntStatus = GoodInt | BadIntEnd


chompInt : String -> Int -> Int -> Int -> (IntStatus, Int, Int)
chompInt src pos end n =
  if pos < end then
    let word = P.unsafeIndex src pos in
    if isDecimalDigit word then
      let m = 10 * n + word - 0x30 {-0-} in
      chompInt src (pos + 1) end m
    else if word == 0x2E {-.-} || word == 0x65 {-e-} || word == 0x45 {-E-} then
      (BadIntEnd, n, pos)
    else
      (GoodInt, n, pos)

  else
    (GoodInt, n, pos)


isDecimalDigit : Int -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}
