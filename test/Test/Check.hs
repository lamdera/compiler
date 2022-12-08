{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Check where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import EasyTest
import Test.Helpers

import Lamdera
import Lamdera.Evergreen.Snapshot
import NeatInterpolation
import qualified Lamdera.CLI.Check
import qualified System.Directory as Dir

import qualified Test.Wire

import LamderaSharedBuildHelpers

all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "production check -> AppConfig usages & injection" $ do
      _ <- io $ checkWithParams "test" "always-v0"

      "test/elm-stuff/lamdera/.lamdera-fe-config" & expectFileContains "configFEOnly"
      "test/elm-stuff/lamdera/.lamdera-be-config" & expectFileContains "configBEOnly"

      "test/elm-stuff/lamdera/.lamdera-fe-config" & expectFileContains "configBoth"
      "test/elm-stuff/lamdera/.lamdera-be-config" & expectFileContains "configBoth"

      "test/frontend-app.js" & expectFileContains "fe-only-from-dashboard"
      "test/backend-app.js" & expectFileContains "be-only-from-dashboard"

      "test/frontend-app.js" & expectFileContains "both-from-dashboard"
      "test/backend-app.js" & expectFileContains "both-from-dashboard"
  ]


expectFileContains :: Text -> FilePath -> Test ()
expectFileContains needle file =
  scope ("-> '" ++ file ++ "' contains '" ++ T.unpack needle ++ "'") $ do
    exists_ <- io $ Dir.doesFileExist file
    onlyWhen (not exists_) $
      crash $ "expectFileContains: file not found: " ++ file

    textM <- io $ readUtf8Text file
    case textM of
      Just text ->
        if (not $ textContains needle text)
          then
            crash $ "expectFileContains: file '" ++ file ++ "' does not contain '" ++ T.unpack needle ++ "'"
          else
            ok

      Nothing ->
        ok


{-| For quick and general local development testing via `stack ghci` as Test.Check.check -}
check = do
  -- touch "/Users/mario/lamdera/test/v1/src/WireTypes.elm"
  -- touch "/Users/mario/lamdera/test/v1/src/Env.elm"
  -- checkWithParams "/Users/mario/lamdera/test/v1" "always-v0"
  -- checkWithParams "/Users/mario/dev/test/ascii-art" "ascii-art-local"
  -- checkWithParams "/Users/mario/dev/test/lamdera-minilatex-app" "minilatex"
  -- checkWithParams "/Users/mario/dev/lamdera-user-projects/beat-the-big-two" "beat-the-big-two"
  checkWithParams "/Users/mario/dev/projects/lamdera-dashboard" "dashboard"
  -- checkWithParams "/Users/mario/dev/projects/lamdera-test" "testapp"
  -- checkWithParams "/Users/mario/lamdera/tmp/elm-audio-test0" "elm-audio-test0"
  -- checkWithParams "/Users/mario/lamdera-builds/build-test-local/staging" "test-local"


mockBuildSh projectPath appName = do

  -- @TODO this doesn't yet fully replicate the build.sh script!
  -- if extracting this for https://trello.com/c/BcomTNnd, make
  -- sure to step through build.sh step-by-step.

  -- c "~/lamdera/scripts/makeDevPackages.sh"


  setEnv "FORCEVERSION" "1"
  setEnv "LDEBUG" "1"
  requireEnv "TOKEN"
  setEnv "LOVR" "/Users/mario/lamdera/overrides"
  setEnv "LAMDERA_APP_NAME" appName
  -- setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
  -- setEnv "NOTPROD" "1"

  -- clearPriorBuilds
  -- clearPriorDeploys
  -- clearSnapshots

  clearBuildCache projectPath

  copyRuntimeFiles projectPath
  injectElmPkgJsIncludesDefaultIfMissing projectPath
  injectFrontendAppConfig projectPath appName "1"
  killAppZero appName

  npmInstall projectPath
  addRPCDefaultIfMissing projectPath
  installElmHttpForRPC projectPath
  rebuildLamderaCheckProd projectPath appName
  parcelFrontendNoMinify projectPath
  -- parcelFrontendMinify projectPath
  clearEnv
  bootNodejsApp projectPath appName


rebuildLamderaCheckProd projectPath appName = do
  launchAppZero $ T.pack appName

  -- FORCEVERSION=1 LDEBUG=1 TOKEN=$TOKEN LOVR=${LOVR} ELM_HOME=$ELM_HOME_ LAMDERA_APP_NAME=${APP} $LAMDERA_COMPILER check # >> $LOG 2>&1
  Dir.withCurrentDirectory projectPath $
    Lamdera.CLI.Check.run () ()

  killAppZero appName


installElmHttpForRPC projectPath = do
  withCurrentDirectory projectPath $ do
    Test.Wire.installHelper Pkg.http



{-| Run the `lamdera check` pipeline with specific params -}
checkWithParams projectPath appName = do
  setEnv "LAMDERA_APP_NAME" appName
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
  -- setEnv "NOTPROD" "1"
  requireEnv "TOKEN"
  -- setEnv "HOIST_REBUILD" "1"
  -- setEnv "VERSION" "1"

  cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaRPC.elm" (projectPath ++ "/src/LamderaRPC.elm")

  rpcExists <- doesFileExist (projectPath ++ "/src/RPC.elm")
  onlyWhen (not rpcExists) $
    cp "/Users/mario/lamdera/runtime/src/RPC_Empty.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  Dir.withCurrentDirectory projectPath $
    do
        Lamdera.CLI.Check.run () ()

  -- rm (projectPath ++ "/src/LBR.elm")
  -- rm (projectPath ++ "/src/LFR.elm")
  -- rm (projectPath ++ "/src/RPC.elm")
  -- rm (projectPath ++ "/src/LamderaHelpers.elm")

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"
  unsetEnv "NOTPROD"
  unsetEnv "TOKEN"
  unsetEnv "VERSION"



{-| Run the `lamdera check` pipeline with specific params -}
checkWithParamsNoDebug version projectPath appName = do
  unsetEnv "LDEBUG"

  setEnv "LAMDERA_APP_NAME" appName
  setEnv "VERSION" $ show version
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
  setEnv "NOTPROD" "1"

  cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  cp "/Users/mario/lamdera/runtime/src/RPC.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  Dir.withCurrentDirectory projectPath $ Lamdera.CLI.Check.run () ()

  rm (projectPath ++ "/src/LBR.elm")
  rm (projectPath ++ "/src/LFR.elm")
  rm (projectPath ++ "/src/RPC.elm")
  rm (projectPath ++ "/src/LamderaHelpers.elm")

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "VERSION"
  unsetEnv "LOVR"
  unsetEnv "ELM_HOME"
  unsetEnv "NOTPROD"
