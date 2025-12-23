{- MANUALLY FORMATTED -}
module Compiler.Parse.Primitives exposing
  ( fromByteString
  , Parser(..)
  , State(..)
  , Row
  , Col
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, addLocation, addEnd
  , withIndent, withBacksetIndent
  , word1, word2
  , unsafeIndex, isWord, getCharWidth
  , Snippet(..)
  , fromSnippet
  --
  , PStep(..)
  , fmap
  , return, bind
  , Step(..), loop
  )


import Compiler.Reporting.Annotation as A
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List exposing (TList)



-- PARSER


type Parser x a =
  Parser (State -> PStep x a)


type PStep x a
  = Cok a State
  | Eok a State
  | Cerr Row Col (Row -> Col -> x)
  | Eerr Row Col (Row -> Col -> x)


type State = -- PERF try taking some out to avoid allocation
  State
    {- src -} String
    {- pos -} Int
    {- end -} Int
    {- indent -} Int
    {- row -} Row
    {- col -} Col


type alias Row = Int
type alias Col = Int



-- FUNCTOR


fmap : Functor.Fmap a (Parser x a) b (Parser x b)
fmap f (Parser parser) =
  Parser <| \state ->
    case parser state of
      Cok a s -> Cok (f a) s
      Eok a s -> Eok (f a) s
      Cerr r c t -> Cerr r c t
      Eerr r c t -> Eerr r c t



-- ONE OF


oneOf : (Row -> Col -> x) -> TList (Parser x a) -> Parser x a
oneOf toError parsers =
  Parser <| \state ->
    oneOfHelp state toError parsers


oneOfHelp
  : State
  -> (Row -> Col -> x)
  -> TList (Parser x a)
  -> PStep x a
oneOfHelp state toError parsers =
  case parsers of
    Parser parser :: parsers_ ->
      case parser state of
        --Cok a s -> Cok a s
        --Eok a s -> Eok a s
        --Cerr r c t -> Cerr r c t
        Eerr _ _ _ -> oneOfHelp state toError parsers_
        x -> x

    [] ->
      let
        (State _ _ _ _ row col) = state
      in
      Eerr row col toError



-- ONE OF WITH FALLBACK


oneOfWithFallback : TList (Parser x a) -> a -> Parser x a -- PERF is this function okay? Worried about allocation/laziness with fallback values.
oneOfWithFallback parsers fallback =
  Parser <| \state ->
    oowfHelp state parsers fallback


oowfHelp
  : State
  -> TList (Parser x a)
  -> a
  -> PStep x a
oowfHelp state parsers fallback =
  case parsers of
    [] ->
      Eok fallback state

    Parser parser :: parsers_ ->
      case parser state of
        --Cok a s -> Cok a s
        --Eok a s -> Eok a s
        --Cerr r c t -> Cerr r c t
        Eerr _ _ _ -> oowfHelp state parsers_ fallback
        x -> x



-- LOOP


type Step state a
  = Loop state
  | Done a


loop : (state -> Parser x (Step state a)) -> state -> Parser x a
loop callback loopState =
  Parser <| \state ->
    loopHelp callback state loopState Eok Eerr


loopHelp
  : (state -> Parser x (Step state a))
  -> State
  -> state
  -> (a -> State -> PStep x a)
  -> (Row -> Col -> (Row -> Col -> x) -> PStep x a)
  -> PStep x a
loopHelp callback state loopState eok eerr =
  case callback loopState of
    Parser parser ->
      case parser state of
        Cok (Loop newLoopState) newState -> loopHelp callback newState newLoopState Cok Cerr
        Cok (Done a) newState -> Cok a newState
        Eok (Loop newLoopState) newState -> loopHelp callback newState newLoopState eok eerr
        Eok (Done a) newState -> eok a newState
        Cerr r c t -> Cerr r c t
        Eerr r c t -> eerr r c t



-- MONAD


return : Monad.Return a (Parser x a)
return value =
  Parser <| \state ->
    Eok value state


bind : Monad.Bind a (Parser x a) (Parser x b)
bind (Parser parserA) callback =
  Parser <| \state ->
    case parserA state of
      Cok a s ->
        case callback a of
          Parser parserB ->
            case parserB s of
              --Cok a_ s_ -> Cok a_ s_
              Eok a_ s_ -> Cok a_ s_
              --Cerr r c t -> Cerr r c t
              Eerr r c t -> Cerr r c t
              x -> x
      Eok a s ->
        case callback a of
          Parser parserB ->
            parserB s
      Cerr r c t -> Cerr r c t
      Eerr r c t -> Eerr r c t



-- FROM BYTESTRING


fromByteString : Parser x a -> (Row -> Col -> x) -> String -> Either x a
fromByteString (Parser parser) toBadEnd fptr =
  let
    toOk_ = toOk toBadEnd
    pos = 0
    end = String.length fptr
    result = parser (State fptr pos end 0 1 1)
  in
  case result of
    Cok a s -> toOk_ a s
    Eok a s -> toOk_ a s
    Cerr r c t -> toErr r c t
    Eerr r c t -> toErr r c t


toOk : (Row -> Col -> x) -> a -> State -> Either x a
toOk toBadEnd a (State _ pos end _ row col) =
  if pos == end
  then Right a
  else Left (toBadEnd row col)


toErr : Row -> Col -> (Row -> Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)



-- FROM SNIPPET


type Snippet =
  Snippet
    {- fptr   -} String
    {- offset -} Int
    {- length -} Int
    {- offRow -} Row
    {- offCol -} Col


fromSnippet : Parser x a -> (Row -> Col -> x) -> Snippet -> Either x a
fromSnippet (Parser parser) toBadEnd (Snippet fptr offset length row col) =
  let
    toOk_ = toOk toBadEnd
    pos = offset
    end = pos + length
    result = parser (State fptr pos end 0 row col)
  in
  case result of
    Cok a s -> toOk_ a s
    Eok a s -> toOk_ a s
    Cerr r c t -> toErr r c t
    Eerr r c t -> toErr r c t



-- POSITION


getPosition : Parser x A.Position
getPosition =
  Parser <| \((State _ _ _ _ row col) as state) ->
    Eok (A.Position row col) state


addLocation : Parser x a -> Parser x (A.Located a)
addLocation (Parser parser) =
  Parser <| \((State _ _ _ _ sr sc) as state) ->
    case parser state of
      Cok a ((State _ _ _ _ er ec) as s) -> Cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
      Eok a ((State _ _ _ _ er ec) as s) -> Eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
      Cerr r c t -> Cerr r c t
      Eerr r c t -> Eerr r c t


addEnd : A.Position -> a -> Parser x (A.Located a)
addEnd start value =
  Parser <| \((State _ _ _ _ row col) as state) ->
    Eok (A.at start (A.Position row col) value) state



-- INDENT


withIndent : Parser x a -> Parser x a
withIndent (Parser parser) =
  Parser <| \(State src pos end oldIndent row col) ->
    case parser (State src pos end col row col) of
      Cok a (State s p e _ r c) -> Cok a (State s p e oldIndent r c)
      Eok a (State s p e _ r c) -> Eok a (State s p e oldIndent r c)
      --Cerr r c t -> Cerr r c t
      --Eerr r c t -> Eerr r c t
      x -> x


withBacksetIndent : Int -> Parser x a -> Parser x a
withBacksetIndent backset (Parser parser) =
  Parser <| \(State src pos end oldIndent row col) ->
    case parser (State src pos end (col - backset) row col) of
      Cok a (State s p e _ r c) -> Cok a (State s p e oldIndent r c)
      Eok a (State s p e _ r c) -> Eok a (State s p e oldIndent r c)
      --Cerr r c t -> Cerr r c t
      --Eerr r c t -> Eerr r c t
      x -> x



-- CONTEXT


inContext : (x -> Row -> Col -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser <| \((State _ _ _ _ row col) as state) ->
    case parserStart state of
      Cok _ s ->
        case parserA s of
          Cok a s_ -> Cok a s_
          Eok a s_ -> Cok a s_
          Cerr r c tx -> Cerr row col (addContext (tx r c))
          Eerr r c tx -> Cerr row col (addContext (tx r c))
      Eok _ s ->
        case parserA s of
          Cok a s_ -> Cok a s_
          Eok a s_ -> Eok a s_
          Cerr r c tx -> Cerr row col (addContext (tx r c))
          Eerr r c tx -> Eerr row col (addContext (tx r c))
      Cerr r c t -> Cerr r c t
      Eerr r c t -> Eerr r c t


specialize : (x -> Row -> Col -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
  Parser <| \((State _ _ _ _ row col) as state) ->
    case parser state of
      Cok a s -> Cok a s
      Eok a s -> Eok a s
      Cerr r c tx -> Cerr row col (addContext (tx r c))
      Eerr r c tx -> Eerr row col (addContext (tx r c))



-- SYMBOLS


word1 : Int -> (Row -> Col -> x) -> Parser x ()
word1 word toError =
  Parser <| \(State src pos end indent row col) ->
    if pos < end && unsafeIndex src pos == word then
      let newState = State src (pos + 1) end indent row (col + 1) in
      Cok () newState
    else
      Eerr row col toError


word2 : Int -> Int -> (Row -> Col -> x) -> Parser x ()
word2 w1 w2 toError =
  Parser <| \(State src pos end indent row col) ->
    let
      pos1 = pos + 1
    in
    if pos1 < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
      let newState = State src (pos + 2) end indent row (col + 2) in
      Cok () newState
    else
      Eerr row col toError



-- LOW-LEVEL CHECKS


unsafeIndex : String -> Int -> Int
unsafeIndex src pos =
  src
    |> String.dropLeft pos
    |> String.uncons
    |> Maybe.map (Tuple.first >> Char.toCode)
    |> Maybe.withDefault 0


isWord : String -> Int -> Int -> Int -> Bool
isWord src pos end word =
  pos < end && unsafeIndex src pos == word


getCharWidth : Int -> Int
getCharWidth word =
  if word > 0xFFFF then 2
  else 1
