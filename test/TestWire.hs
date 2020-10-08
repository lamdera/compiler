{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module TestWire where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))

import EasyTest
import Lamdera
import Lamdera.Snapshot
import NeatInterpolation
-- import qualified TestLamdera
import qualified Lamdera.Compile


all = EasyTest.run suite

suite :: Test ()
suite = tests $
  [ scope "compile all Elm wire expectations" $ do
      io wire
  ]

wire :: IO ()
wire = do
  let project = "/Users/mario/dev/projects/elmx/test"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LTEST" "1"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  -- Dir.withCurrentDirectory project $
  --   tests <- safeListDirectory "src/Test"
  --   @TODO in future?

  let testFiles =
        [ "src/Test/Wire_Union_1_Basic.elm"
        , "src/Test/Wire_Union_2_Basic.elm"
        , "src/Test/External.elm"
        , "src/Test/Wire_Union_3_Params.elm"
        , "src/Test/Wire_Union_4_Tricky.elm"
        , "src/Test/Wire_Alias_1_Basic.elm"
        , "src/Test/Wire_Alias_2_Record.elm"
        , "src/Test/Wire_Alias_3_SubAlias.elm"
        ]

  testFiles & mapM (\filename -> do
      -- Bust Elm's caching with this one weird trick!
      touch $ project </> filename
      Lamdera.Compile.make project (project </> filename)
    )

  unsetEnv "LOVR"
  unsetEnv "LTEST"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"
