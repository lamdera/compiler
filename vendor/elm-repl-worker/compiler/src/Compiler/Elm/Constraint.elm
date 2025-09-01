{- MANUALLY FORMATTED -}
module Compiler.Elm.Constraint exposing
  ( Constraint
  , exactly
  , anything
  , toChars
  , satisfies
  --, check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  --, expand
  --
  , Error(..)
  , decoder
  , encode
  )


import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P exposing (Row, Col)
import Extra.Type.List as MList



-- CONSTRAINTS


type Constraint
  = Range V.Version Op Op V.Version


type Op
  = Less
  | LessOrEqual



-- COMMON CONSTRAINTS


exactly : V.Version -> Constraint
exactly version =
  Range version LessOrEqual LessOrEqual version


anything : Constraint
anything =
  Range V.one LessOrEqual LessOrEqual V.max



-- TO CHARS


toChars : Constraint -> String
toChars constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      V.toChars lower ++ opToChars lowerOp ++ "v" ++ opToChars upperOp ++ V.toChars upper


opToChars : Op -> String
opToChars op =
  case op of
    Less        -> " < "
    LessOrEqual -> " <= "



-- IS SATISFIED


satisfies : Constraint -> V.Version -> Bool
satisfies constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      isLess lowerOp (V.toComparable lower) (V.toComparable version)
        &&
      isLess upperOp (V.toComparable version) (V.toComparable upper)


isLess : Op -> (comparable -> comparable -> Bool)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)



-- INTERSECT


intersect : Constraint -> Constraint -> Maybe Constraint
intersect (Range lo lop hop hi) (Range lo_ lop_ hop_ hi_) =
  let
    (newLo, newLop) =
      case compare (V.toComparable lo) (V.toComparable lo_) of
        LT -> (lo_, lop_)
        EQ -> (lo, if MList.elem Less [lop,lop_] then Less else LessOrEqual)
        GT -> (lo, lop)

    (newHi, newHop) =
      case compare (V.toComparable hi) (V.toComparable hi_) of
        LT -> (hi, hop)
        EQ -> (hi, if MList.elem Less [hop, hop_] then Less else LessOrEqual)
        GT -> (hi_, hop_)
  in
    if (V.toComparable newLo) <= (V.toComparable newHi) then
      Just (Range newLo newLop newHop newHi)
    else
      Nothing



-- ELM CONSTRAINT


goodElm : Constraint -> Bool
goodElm constraint =
  satisfies constraint V.compiler


defaultElm : Constraint
defaultElm =
  if V.getMajor V.compiler > 0
    then untilNextMajor V.compiler
    else untilNextMinor V.compiler



-- CREATE CONSTRAINTS


untilNextMajor : V.Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (V.bumpMajor version)


untilNextMinor : V.Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (V.bumpMinor version)



-- JSON


encode : Constraint -> E.Value
encode constraint =
  E.chars (toChars constraint)


decoder : D.Decoder Error Constraint
decoder =
  D.customString parser BadFormat



-- PARSER


type Error
  = BadFormat Row Col
  | InvalidRange V.Version V.Version


parser : P.Parser Error Constraint
parser =
  P.bind parseVersion <| \lower ->
  P.bind (P.word1 0x20 {- -} BadFormat) <| \_ ->
  P.bind parseOp <| \loOp ->
  P.bind (P.word1 0x20 {- -} BadFormat) <| \_ ->
  P.bind (P.word1 0x76 {-v-} BadFormat) <| \_ ->
  P.bind (P.word1 0x20 {- -} BadFormat) <| \_ ->
  P.bind parseOp <| \hiOp ->
  P.bind (P.word1 0x20 {- -} BadFormat) <| \_ ->
  P.bind parseVersion <| \higher ->
  P.Parser <| \((P.State _ _ _ _ row col) as state) ->
    if V.toComparable lower < V.toComparable higher
    then P.Eok (Range lower loOp hiOp higher) state
    else P.Eerr row col (\_ _ -> InvalidRange lower higher)


parseVersion : P.Parser Error V.Version
parseVersion =
  P.specialize (\(r,c) _ _ -> BadFormat r c) V.parser


parseOp : P.Parser Error Op
parseOp =
  P.bind (P.word1 0x3C {-<-} BadFormat) <| \_ ->
  P.oneOfWithFallback
    [ P.bind (P.word1 0x3D {-=-} BadFormat) <| \_ ->
      P.return LessOrEqual
    ]
    Less
