{-# LANGUAGE OverloadedStrings #-}

module Test.MultiVersion where

{- Elm 0.19.1, 0.19.2 and 0.19.3 are interchangeable for Lamdera on both sides of
elm.json: the application's own elm-version (Elm.Details.isCompatibleAppElmVersion)
and every package's elm-version bound (Elm.Constraint.goodElm, via
interchangeableElm). This checks the two predicates directly;
test/multi-version/run.sh builds real applications against rewritten bounds.
-}

import qualified Data.ByteString.Char8 as BS

import EasyTest

import qualified Elm.Constraint as Con
import qualified Elm.Details as Details
import qualified Elm.Version as V
import qualified Json.Decode as D


suite :: Test ()
suite = tests
  [ scope "the compiler is Elm 0.19.3" $
      expectEqual "0.19.3" (V.toChars V.compiler)

  , scope "applications at 0.19.1, 0.19.2 and 0.19.3 are accepted" $
      expectEqual [True, True, True] $
        fmap Details.isCompatibleAppElmVersion [V.toVersion 0 19 1, V.toVersion 0 19 2, V.toVersion 0 19 3]

  , scope "applications past the compiler, or outside 0.19, are refused" $
      expectEqual [False, False, False] $
        fmap Details.isCompatibleAppElmVersion [V.toVersion 0 19 4, V.toVersion 0 20 0, V.toVersion 1 0 0]

  , scope "package bounds starting at 0.19.0, 0.19.1, 0.19.2 and 0.19.3 are accepted" $
      goodElm True
        [ "0.19.0 <= v < 0.20.0"
        , "0.19.1 <= v < 0.20.0"
        , "0.19.2 <= v < 0.20.0"
        , "0.19.3 <= v < 0.20.0"
        ]

  , scope "package bounds that exclude the compiler but admit 0.19.1 or 0.19.2 are accepted" $
      goodElm True
        [ "0.19.0 <= v < 0.19.2" -- bburdette/toop 1.2.0
        , "0.19.0 <= v < 0.19.3"
        , "0.19.1 <= v < 0.19.2"
        , "0.19.2 <= v < 0.19.3"
        ]

  , scope "package bounds that admit none of 0.19.1, 0.19.2 and 0.19.3 are refused" $
      goodElm False
        [ "0.19.0 <= v < 0.19.1"
        , "0.18.0 <= v < 0.19.0"
        , "0.19.4 <= v < 0.20.0"
        , "0.20.0 <= v < 0.21.0"
        ]
  ]


goodElm :: Bool -> [String] -> Test ()
goodElm expected bounds =
  do  results <- traverse (\bound -> (,) bound . Con.goodElm <$> constraint bound) bounds
      expectEqual (fmap (\bound -> (bound, expected)) bounds) results


constraint :: String -> Test Con.Constraint
constraint bound =
  do  result <- io $ D.fromByteString Con.decoder (BS.pack (show bound))
      case result of
        Right c -> pure c
        Left _ -> crash $ "could not parse the constraint " ++ show bound
