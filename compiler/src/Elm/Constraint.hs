{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings #-}
module Elm.Constraint
  ( Constraint(..) -- @LAMDERA exposed
  , exactly
  , anything
  , toChars
  , satisfies
  , check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  , expand
  --
  , Error(..)
  , decoder
  , encode
  --
  , dConstraint, eConstraint
  )
  where


import Control.Monad (liftM4)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified Elm.Version as V
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Parse.Primitives as P
import Parse.Primitives (Cursor)
import qualified Reporting.Annotation as A



-- CONSTRAINTS


data Constraint
    = Range V.Version Op Op V.Version
    deriving (Eq, Show)


data Op
  = Less
  | LessOrEqual
  deriving (Eq, Show)



-- COMMON CONSTRAINTS


exactly :: V.Version -> Constraint
exactly version =
  Range version LessOrEqual LessOrEqual version


anything :: Constraint
anything =
  Range V.one LessOrEqual LessOrEqual V.max



-- TO CHARS


toChars :: Constraint -> [Char]
toChars constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      V.toChars lower ++ opToChars lowerOp ++ "v" ++ opToChars upperOp ++ V.toChars upper


opToChars :: Op -> [Char]
opToChars op =
  case op of
    Less        -> " < "
    LessOrEqual -> " <= "



-- IS SATISFIED


satisfies :: Constraint -> V.Version -> Bool
satisfies constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: (Ord a) => Op -> (a -> a -> Bool)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)


check :: Constraint -> V.Version -> Ordering
check constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      if not (isLess lowerOp lower version) then
        LT

      else if not (isLess upperOp version upper) then
        GT

      else
        EQ



-- INTERSECT


intersect :: Constraint -> Constraint -> Maybe Constraint
intersect (Range lo lop hop hi) (Range lo_ lop_ hop_ hi_) =
  let
    (newLo, newLop) =
      case compare lo lo_ of
        LT -> (lo_, lop_)
        EQ -> (lo, if elem Less [lop,lop_] then Less else LessOrEqual)
        GT -> (lo, lop)

    (newHi, newHop) =
      case compare hi hi_ of
        LT -> (hi, hop)
        EQ -> (hi, if elem Less [hop, hop_] then Less else LessOrEqual)
        GT -> (hi_, hop_)
  in
    if newLo <= newHi then
      Just (Range newLo newLop newHop newHi)
    else
      Nothing



-- ELM CONSTRAINT


-- @LAMDERA A package's elm.json carries its OWN `elm-version` bound, and this is
-- where it is checked against the running compiler. Elm 0.19.2 is a minor release
-- with no language or format changes, so a package published against 0.19.1 is fine
-- on 0.19.2 and vice versa - but a package whose bound happens to EXCLUDE 0.19.2
-- (bburdette/toop 1.2.0 declares "0.19.0 <= v < 0.19.2") is refused outright, and the
-- app depending on it fails to solve with Exit.DetailsNoSolution, which prints the
-- whole constraint set without naming which constraint was unsatisfiable.
--
-- Details.isCompatibleAppElmVersion already makes the APPLICATION side of elm.json
-- flexible across 0.19.x; this is the package side of the same idea, without which
-- that flexibility only half works.
--
-- Deliberately an EXPLICIT PAIR rather than "any 0.19.x". Interchangeability is a
-- claim about two specific releases that have been tested against each other, not a
-- property 0.19.x versions have in general. A future 0.19.3 will NOT be accepted here
-- by default: including it has to be a deliberate choice someone makes after
-- checking, which is exactly what an open range would take away.
--
-- 0.19.3 was added deliberately when Lamdera moved onto it: its changes are internal
-- to the compiler (precise name types, a new binary cache format) and leave the
-- language, elm.json and package formats alone. Packages bounded at 0.19.0, 0.19.1,
-- 0.19.2 and 0.19.3 resolve interchangeably (see test/multi-version).
interchangeableElm :: [V.Version]
interchangeableElm =
  [ V.toVersion 0 19 1
  , V.toVersion 0 19 2
  , V.toVersion 0 19 3
  ]


goodElm :: Constraint -> Bool
goodElm constraint =
  satisfies constraint V.compiler
  ||
  -- Only substitute while the compiler is ITSELF one of the interchangeable pair, so
  -- this widens nothing for any other compiler version.
  ( V.compiler `elem` interchangeableElm
    && any (satisfies constraint) interchangeableElm
  )


defaultElm :: Constraint
defaultElm =
  V.fromVersion V.compiler $ \major _ _ ->
    if major > 0
    then untilNextMajor V.compiler
    else untilNextMinor V.compiler



-- CREATE CONSTRAINTS


untilNextMajor :: V.Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (V.bumpMajor version)


untilNextMinor :: V.Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (V.bumpMinor version)


expand :: Constraint -> V.Version -> Constraint
expand constraint@(Range lower lowerOp upperOp upper) version
  | version < lower =
      Range version LessOrEqual upperOp upper

  | version > upper =
      Range lower lowerOp Less (V.bumpMajor version)

  | otherwise =
      constraint



-- JSON


encode :: Constraint -> JE.Value
encode constraint =
  JE.chars (toChars constraint)


decoder :: JD.Decoder Error Constraint
decoder =
  JD.customString parser BadFormat



-- BINARY


dConstraint :: D.Decoder Constraint
dConstraint =
  liftM4 Range V.dVersion dOp dOp V.dVersion


eConstraint :: Constraint -> E.Builder
eConstraint (Range lo op op' hi) =
  V.eVersion lo <> eOp op <> eOp op' <> V.eVersion hi


dOp :: D.Decoder Op
dOp =
  do  n <- D.u8
      case n of
        0 -> pure Less
        1 -> pure LessOrEqual
        _ -> D.expecting "Op"


eOp :: Op -> E.Builder
eOp op =
  case op of
    Less        -> E.u8# 0#Word8
    LessOrEqual -> E.u8# 1#Word8



-- PARSER


data Error
  = BadFormat Cursor
  | InvalidRange V.Version V.Version


parser :: P.Parser Error Constraint
parser =
  do  lower <- parseVersion
      P.word1 0x20#Word8 {- -} BadFormat
      loOp <- parseOp
      P.word1 0x20#Word8 {- -} BadFormat
      P.word1 0x76#Word8 {-v-} BadFormat
      P.word1 0x20#Word8 {- -} BadFormat
      hiOp <- parseOp
      P.word1 0x20#Word8 {- -} BadFormat
      higher <- parseVersion
      P.Parser $ \_ state@(P.State _ _ _ cur) _ eok _ eerr ->
        if lower < higher
        then eok (Range lower loOp hiOp higher) state
        else eerr cur (\_ -> InvalidRange lower higher)


parseVersion :: P.Parser Error V.Version
parseVersion =
  P.specialize (\(A.Position c) _ -> BadFormat c) V.parser


parseOp :: P.Parser Error Op
parseOp =
  do  P.word1 0x3C#Word8 {-<-} BadFormat
      P.oneOfWithFallback
        [ do  P.word1 0x3D#Word8 {-=-} BadFormat
              return LessOrEqual
        ]
        Less
