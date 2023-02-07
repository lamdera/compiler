{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Wire where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Utf8 as Utf8

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import EasyTest
import Lamdera
import Lamdera.Evergreen.Snapshot
import NeatInterpolation
-- import qualified TestLamdera
import qualified Lamdera.Compile

import StandaloneInstances

-- Test all
import qualified Reporting.Exit as Exit
import qualified System.Exit as Exit
import qualified Reporting.Task as Task
import qualified Deps.Solver as Solver
import qualified Deps.Registry

import qualified BackgroundWriter as BW
import qualified Elm.Outline as Outline
import qualified Elm.Details as Details
import qualified Install
import qualified Reporting.Task as Task
import qualified Reporting.Doc as D
import qualified Elm.Version as V
import qualified Elm.Constraint as C
import qualified Stuff
import qualified Reporting

import qualified Init
import qualified Make

-- http
import qualified Json.Decode as D
import qualified Lamdera.Http
import qualified Ext.Common

all = EasyTest.run suite

suite :: Test ()
suite = tests $
  [ scope "compile all Elm wire expectations" wire
  ]

wire :: Test ()
wire = do

  failuresM <- io $ newMVar []

  io $ do
    let project = "./test/scenario-alltypes"

    setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
    setEnv "LTEST" "1"
    setEnv "LDEBUG" "1"
    setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

    let testFiles =
          [
            "src/Test/Wire_Union_1_Basic.elm"
          , "src/Test/Wire_Union_2_Basic.elm"
          , "src/Test/External.elm"
          , "src/Test/Wire_Union_3_Params.elm"
          , "src/Test/Wire_Union_4_Tricky.elm"
          , "src/Test/Wire_Alias_1_Basic.elm"
          , "src/Test/Wire_Alias_2_Record.elm"
          , "src/Test/Wire_Alias_3_SubAlias.elm"
          , "src/Test/Wire_Alias_4_TvarRename.elm"
          , "src/Test/Wire_Tvar_Ambiguous.elm"
          , "src/Test/Wire_Core_Types.elm"
          , "src/Test/Wire_Recursive.elm"
          , "src/Test/Wire_Record_Extensible1_Basic.elm"
          , "src/Test/Wire_Record_Extensible2_MultiParam.elm"
          , "src/Test/Wire_Record_Extensible3_Tricky.elm"
          , "src/Test/Wire_Phantom.elm"
          , "src/Test/Wire_Tvar_Deep.elm"
          , "src/Test/Wire_Tvar_Deep2.elm"
          , "src/Test/Wire_Tvar_Recursive_Reference.elm"
          , "src/Test/Wire_Unsupported.elm"
          , "src/Test/Wire_Unconstructable.elm"
          ]

    let
      catchTestException :: FilePath -> SomeException -> IO a
      catchTestException filename e = do
        modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
        putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
        throw e


    testFiles & mapM (\filename -> do
        putStrLn $ "testing: " <> show filename
        -- Bust Elm's caching with this one weird trick!
        touch $ project </> filename
        Lamdera.Compile.makeDev project [filename] `catch` catchTestException filename
      )

    unsetEnv "LOVR"
    unsetEnv "LTEST"
    unsetEnv "LDEBUG"
    unsetEnv "ELM_HOME"

  failures <- io $ readMVar failuresM
  if length failures > 0
    then
      crash failures
    else
      scope "senarios-alltypes no exceptions" $ ok





