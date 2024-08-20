{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Check where

import System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import EasyTest
import NeatInterpolation

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Test.Helpers
import Lamdera
import Lamdera.Evergreen.Snapshot
import qualified Lamdera.CLI.Check
import qualified Lamdera.Offline
import LamderaSharedBuildHelpers
import qualified Ext.Common
import qualified Lamdera.Compile
import qualified Lamdera.Relative


all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "Lamdera.Nitpick.DebugLog - lamdera check" $ do

      project <- io $ Lamdera.Relative.requireDir "test/project-scenarios/blank-injectable"

      io $ Ext.Common.bash $ "cd " <> project <> " && git init"
      io $ Ext.Common.bash $ "cd " <> project <> " && git remote add lamdera git@apps.lamdera.com:always-v0.git"

      let path = (project </> "src" </> "Frontend.elm")

      withFileModifications (project </> "src" </> "Frontend.elm")
        [ ("{- viewFunctionBodyPlaceholder -}",   "_ = Debug.log \"hello\" \"world\"")
        , ("{- updateFunctionBodyPlaceholder -}", "_ = Debug.log \"hello\" \"world\"")
        ] $ do

        scope "lamdera check should succeed, ignoring the Debug.log usages" $ do
          actual <- catchOutput $ checkWithParamsProduction project "always-v0"
          expectTextContains actual
            "It appears you're all set to deploy the first version of 'always-v0'!"

        scope "lamdera make --optimized should continue to fail" $ do
          actual <- catchOutput $ Lamdera.Compile.makeOptimizedWithCleanup (pure ()) project "src/Frontend.elm"
          expectTextContains actual
            "There are uses of the `Debug` module in the following modules:"


  , scope "production check -> AppConfig usages & injection" $ do
      let project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-always-v0"
          expectContains needle file = (project </> file) & expectFileContains needle

      io $ Ext.Common.bash $ "cd " <> project <> " && git init"
      io $ Ext.Common.bash $ "cd " <> project <> " && git remote add lamdera git@apps.lamdera.com:always-v0.git"

      _ <- io $ checkWithParamsProduction project "always-v0"

      "elm-stuff/lamdera/.lamdera-fe-config" & expectContains "frontendOnly"
      "elm-stuff/lamdera/.lamdera-be-config" & expectContains "backendOnly"

      "elm-stuff/lamdera/.lamdera-fe-config" & expectContains "both"
      "elm-stuff/lamdera/.lamdera-be-config" & expectContains "both"

      "frontend-app.js" & expectContains "fe-only-from-dashboard"
      "backend-app.js" & expectContains "be-only-from-dashboard"

      "frontend-app.js" & expectContains "both-from-dashboard"
      "backend-app.js" & expectContains "both-from-dashboard"

      io $ rmdir (project </> ".git")
      io $ cleanupCheckGen project
  ]


withFileModifications :: FilePath -> [(Text, Text)] -> Test a -> Test a
withFileModifications path mods action =
    EasyTest.using
      (mods & mapM_ (\(before, after) -> replaceInFile before after path))
      (\_ -> mods & mapM_ (\(before, after) -> replaceInFile after before path))
      (\_ -> action)


expectFileContains :: Text -> FilePath -> Test ()
expectFileContains needle file =
  scope ("-> '" ++ file ++ "' contains '" ++ T.unpack needle ++ "'") $ do
    exists_ <- io $ Dir.doesFileExist file
    onlyWhen (not exists_) $
      crash $ "❌  expectFileContains: file not found: " ++ file

    textM <- io $ readUtf8Text file
    case textM of
      Just t ->
        if (not $ textContains needle t)
          then
            crash $ "❌  expectFileContains: file '" ++ file ++ "' does not contain '" ++ T.unpack needle ++ "'\n📖 file contents were:\n" ++ T.unpack t
          else
            ok

      Nothing ->
        ok


{-| For quick and general local development testing via `stack ghci` as Test.Check.check -}
check = do
  -- touch "/Users/mario/lamdera/test/v1/src/WireTypes.elm"
  -- touch "/Users/mario/lamdera/test/v1/src/Env.elm"
  -- checkWithParamsProduction "/Users/mario/lamdera/test/v1" "always-v0"
  -- checkWithParamsProduction "/Users/mario/dev/test/ascii-art" "ascii-art-local"
  -- checkWithParamsProduction "/Users/mario/dev/test/lamdera-minilatex-app" "minilatex"
  -- checkWithParamsProduction "/Users/mario/dev/lamdera-user-projects/beat-the-big-two" "beat-the-big-two"
  -- checkWithParamsProduction "/Users/mario/dev/projects/lamdera-dashboard" "dashboard"
  -- checkWithParamsProduction "/Users/mario/dev/projects/lamdera-test" "testapp"
  -- checkWithParamsProduction "/Users/mario/lamdera/tmp/elm-audio-test0" "elm-audio-test0"
  -- checkWithParamsProduction "/Users/mario/lamdera-builds/build-test-local/staging" "test-local"
  checkWithParamsProduction "/Users/mario/dev/projects/bento-life" "life"


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
  Ext.Common.withProjectRoot projectPath $
    Lamdera.CLI.Check.run_

  killAppZero appName


installElmHttpForRPC projectPath = do
  Ext.Common.withProjectRoot projectPath $ do
    Lamdera.Offline.installHelper Pkg.http



{-| Run the `lamdera check` pipeline with specific params -}
checkWithParams projectPath = do
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  Ext.Common.withProjectRoot projectPath $ Lamdera.CLI.Check.run_

  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"


{-| Run the `lamdera check` pipeline as if it were run in production to invoke prod behaviour -}
checkWithParamsProduction projectPath appName = do
  setEnv "LAMDERA_APP_NAME" appName
  -- setEnv "NOTPROD" "1"
  -- setEnv "HOIST_REBUILD" "1"
  -- setEnv "VERSION" "1"

  requireEnv "TOKEN"
  cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaRPC.elm" (projectPath ++ "/src/LamderaRPC.elm")

  rpcExists <- doesFileExist (projectPath ++ "/src/RPC.elm")
  onlyWhen (not rpcExists) $
    cp "/Users/mario/lamdera/runtime/src/RPC_Empty.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  checkWithParams projectPath
  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "NOTPROD"
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

  Ext.Common.withProjectRoot projectPath $ Lamdera.CLI.Check.run_

  rm (projectPath ++ "/src/LBR.elm")
  rm (projectPath ++ "/src/LFR.elm")
  rm (projectPath ++ "/src/RPC.elm")
  rm (projectPath ++ "/src/LamderaHelpers.elm")

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "VERSION"
  unsetEnv "LOVR"
  unsetEnv "ELM_HOME"
  unsetEnv "NOTPROD"


cleanupCheckGen project = do
  rm (project ++ "/src/LBR.elm")
  rm (project ++ "/src/LFR.elm")
  -- rm (project ++ "/src/RPC.elm")
  rm (project ++ "/src/LamderaGenerated.elm")
  rm (project ++ "/src/LamderaHelpers.elm")
  rm (project ++ "/src/LamderaRPC.elm")
  rm (project ++ "/backend-app.js")
  rm (project ++ "/frontend-app.js")
