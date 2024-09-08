{-# LANGUAGE OverloadedStrings #-}

module Test.Lamdera where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T

import Lamdera
import EasyTest

import qualified Init
import qualified Lamdera.CLI.Login
import qualified Lamdera.AppConfig
import qualified Lamdera.Update
import qualified Lamdera.Compile
import qualified Lamdera.Evergreen.Snapshot
import qualified Lamdera.Relative
import Test.Helpers
import Test.Check

import qualified Ext.Common


all = run Test.Lamdera.suite

suite :: Test ()
suite = tests
  [ scope "init should work as expected" $
      let
        tmpFolder = "tmp/new"

        setup = do
          rmdir tmpFolder
          mkdir tmpFolder

        cleanup _ = do
          rmdir tmpFolder

        test _ = do

          actual <- catchOutput $ withStdinYesAll $ Ext.Common.withProjectRoot tmpFolder $ Init.run () ()

          io $ formatHaskellValue "actual" actual

          expectTextContains actual
            "Hello! Lamdera projects always start with an elm.json file, as well as four\\nsource files: Frontend.elm , Backend.elm , Types.elm and Env.elm\\n\\nIf you're new to Elm, the best starting point is\\n<https://elm-lang.org/0.19.1/init>\\n\\nOtherwise check out <https://dashboard.lamdera.app/docs/building> for Lamdera\\nspecific information!\\n\\nKnowing all that, would you like me to create a starter implementation? [Y/n]: Okay, I created it! Now read those links, or get going with `lamdera live`.\\n"

      in
      using setup cleanup test

  , pending $ scope "warning about external packages" $ do
      actual <- catchOutput $ Test.Check.checkWithParamsNoDebug 1 "~/lamdera/test/v1" "always-v0"

      expectTextContains actual
        "It appears you're all set to deploy the first version of 'test-local'!"

      expectTextContains actual
        "WARNING: Evergreen does not cover type changes outside your project yet\\ESC[0m\\n\\nYou are referencing the following in your core types:\\n\\n- Browser.Navigation.Key (elm/browser)\\n- Http.Error (elm/http)\\n- Time.Posix (elm/time)\\n\\n\\ESC[91mPackage upgrades that change these types won't get covered by Evergreen\\nmigrations currently!\\ESC[0m\\n\\nSee <https://dashboard.lamdera.app/docs/evergreen> for more info."

  ]


{-| Run the type snapshot part of `lamdera check` only, with specific params -}
snapshotWithParams :: Int -> FilePath -> String -> IO ()
snapshotWithParams version projectPath appName = do
  project <- Lamdera.Relative.requireDir projectPath
  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

  withEnvVars [("LDEBUG", "1"), ("LAMDERA_APP_NAME", appName), ("LOVR", overrides), ("ELM_HOME", elmHome)] $ do
    Ext.Common.withProjectRoot project $ do
      Lamdera.Compile.makeDev project ["src" </> "Types.elm"]
      debug $ "🎉 runinng snapshot in folder " <> project
      Lamdera.Evergreen.Snapshot.run version



{-| Another test harness for local development -}
testWire = do
  project <- Lamdera.Relative.requireDir "~/lamdera/test/v1"

  -- Bust Elm's caching with this one weird trick!
  touch $ project </> "src/Types.elm"
  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

  let rootPaths = [ "src" </> "Frontend.elm" ]

  withEnvVars [
    ("LAMDERA_APP_NAME", "testapp"),
    ("LOVR", overrides),
    ("LDEBUG", "1"),
    ("ELM_HOME", elmHome)
    ] $
    Lamdera.Compile.makeDev project ["src" </> "Frontend.elm"]


config = do
  -- setEnv "LDEBUG" "1"
  project <- Lamdera.Relative.requireDir "~/lamdera/test/v1"
  withEnvVars [("LDEBUG", "1")] $ do
    Ext.Common.withProjectRoot project $ do
      prodToken <- requireEnv "TOKEN"
      Lamdera.AppConfig.checkUserConfig "test-local" (Just $ T.pack prodToken)


http = do
  res <- Lamdera.Update.fetchCurrentVersion
  putStrLn $ show res


login = do
  project <- Lamdera.Relative.requireDir "~/lamdera/test/v1"
  withEnvVars [("LDEBUG", "1")] $ do

    Ext.Common.withProjectRoot project $
      Lamdera.CLI.Login.run () ()


compileCore = do
  withDebug $ do
    project <- Lamdera.Relative.requireDir "~/lamdera/overrides/packages/lamdera/core/1.0.0"
    aggressiveCacheClear project
    Lamdera.Compile.make_ project


compileCodecs =
  withDebug $ do
    project <- Lamdera.Relative.requireDir "~/lamdera/overrides/packages/lamdera/codecs/1.0.0"
    aggressiveCacheClear project
    Lamdera.Compile.make_ project


checkUserConfig = do
  projectPath <- Lamdera.Relative.requireDir "~/lamdera/test/v1"
  let appName = "always-v0"
  adminToken <- requireEnv "TOKEN"

  withEnvVars [("LDEBUG", "1")] $ do
    Ext.Common.withProjectRoot projectPath $
      do
          Lamdera.Compile.makeHarnessDevJs projectPath
          Lamdera.AppConfig.checkUserConfig appName (Just (T.pack adminToken))
