{-# LANGUAGE OverloadedStrings #-}

module Test.Lamdera where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T
import System.Environment (setEnv, unsetEnv, lookupEnv)

import Lamdera
import EasyTest

import qualified Init
import qualified Lamdera.CLI.Login
import qualified Lamdera.AppConfig
import qualified Lamdera.Update
import qualified Lamdera.Compile
import qualified Lamdera.Evergreen.Snapshot
import Test.Helpers
import Test.Check

import LamderaSharedBuildHelpers (adminToken)

-- import qualified Lamdera.CLI.Check
-- import qualified Lamdera.CLI.Reset
-- import qualified Lamdera.CLI.Live
-- import qualified Lamdera.ReverseProxy
-- import Test.Wire


import Test.Main (captureProcessResult)

all = run Test.Lamdera.suite

suite :: Test ()
suite = tests
  [ pending $ scope "init should write .gitignore" $
      let
        tmpFolder = "tmp/new"

        setup = do
          rmdir tmpFolder
          mkdir tmpFolder

        cleanup _ = do
          rmdir tmpFolder

        test _ = do

          actual <- catchOutput $ withStdinYesAll $ Dir.withCurrentDirectory tmpFolder $ Init.run () ()

          io $ formatHaskellValue "actual" actual

          expectTextContains actual
            "Hello! Lamdera projects always start with an elm.json file, as well as three\\nsource files: Frontend.elm , Backend.elm and Types.elm\\n\\nIf you're new to Elm, the best starting point is\\n<https://elm-lang.org/0.19.0/init>\\n\\nOtherwise check out <https://dashboard.lamdera.app/docs/building> for Lamdera\\nspecific information!\\n\\nKnowing all that, would you like me to create a starter implementation? [Y/n]: Okay, I created it! Now read those links, or get going with `lamdera live`.\\n"

          ignoreM <- io $ readUtf8Text $ tmpFolder </> ".gitignore"

          case ignoreM of
            Just ignore ->
              expectTextContains ignore "elm-stuff"

            Nothing ->
              crash $ "Expected to find " <> tmpFolder <> "/.gitignore but didn't."

      in
      using setup cleanup test

  , pending $ scope "warning about external packages" $ do
      actual <- catchOutput $ Test.Check.checkWithParamsNoDebug 1 "/Users/mario/lamdera/test/v1" "test-local"

      -- io $ formatHaskellValue "actual" actual

      expectTextContains actual
        "It appears you're all set to deploy the first version of 'test-local'!"

      expectTextContains actual
        "WARNING: Evergreen Alpha does not cover type changes outside your project\\ESC[0m\\n\\nYou are referencing the following in your core types:\\n\\n- Browser.Navigation.Key (elm/browser)\\n- Http.Error (elm/http)\\n- Time.Posix (elm/time)\\n\\n\\ESC[91mPackage upgrades that change these types won't get covered by Evergreen\\nmigrations currently!\\ESC[0m\\n\\nSee <https://dashboard.lamdera.app/docs/evergreen> for more info."

  ]


catchOutput :: IO () -> Test Text
catchOutput action = do
  -- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
  pr <- io $ captureProcessResult action
  -- @TODO improve this to actually pull out values
  pure $ show_ pr


compile :: IO ()
compile = do
  let project = "/Users/mario/lamdera/test/v1"
  -- setEnv "LAMDERA_APP_NAME" "testapp"
  setEnv "TOKEN" adminToken

  -- let project = "/Users/mario/dev/projects/elm-spa-example"
  -- setEnv "LAMDERA_APP_NAME" "realworldish"

  -- let project = "/Users/mario/dev/projects/lamdera-test"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/tmp/lamdera-experiments"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/dev/projects/lamdera-dashboard"
  -- setEnv "LAMDERA_APP_NAME" "dashboard"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  -- Bust Elm's caching with this one weird trick!
  touch $ project </> "src/Frontend.elm"
  touch $ project </> "src/Types.elm"
  touch $ project </> "src/WireTypes.elm"

  Lamdera.Compile.make project ("src" </> "Frontend.elm")

  unsetEnv "TOKEN"
  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"



{-| Run the type snapshot part of `lamdera check` only, with specific params -}
snapshotWithParams :: Int -> FilePath -> String -> IO ()
snapshotWithParams version projectPath appName = do
  setEnv "LAMDERA_APP_NAME" appName
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  Dir.withCurrentDirectory projectPath $ do
    Lamdera.Compile.make projectPath ("src" </> "Types.elm")
    Lamdera.Evergreen.Snapshot.run version

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"


{-| Another test harness for local development -}
testWire = do
  let project = "/Users/mario/lamdera/test/v1"
  setEnv "LAMDERA_APP_NAME" "testapp"

  -- let project = "/Users/mario/dev/projects/elm-spa-example"
  -- setEnv "LAMDERA_APP_NAME" "realworldish"

  -- let project = "/Users/mario/dev/projects/lamdera-test"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/tmp/lamdera-experiments"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/dev/projects/lamdera-dashboard"
  -- setEnv "LAMDERA_APP_NAME" "dashboard"

  -- Bust Elm's caching with this one weird trick!
  touch $ project </> "src/Types.elm"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  let rootPaths = [ "src" </> "Frontend.elm" ]

  Lamdera.Compile.make project ("src" </> "Frontend.elm")


config = do
  -- setEnv "LDEBUG" "1"
  let project = "/Users/mario/lamdera/test/v1"
  -- let project = "/Users/mario/dev/projects/lamdera-test"
  Dir.withCurrentDirectory project $ do
    setEnv "TOKEN" adminToken

    prodTokenM <- lookupEnv "TOKEN"
    Lamdera.AppConfig.checkUserConfig "test-local" (fmap T.pack prodTokenM)

    unsetEnv "TOKEN"
    unsetEnv "LDEBUG"


http = do
  res <- Lamdera.Update.fetchCurrentVersion
  putStrLn $ show res


login = do
  let project = "/Users/mario/lamdera/test/v1"
  setEnv "LDEBUG" "1"
  -- setEnv "LAMDERA_APP_NAME" "test-local"
  Dir.withCurrentDirectory project $
    Lamdera.CLI.Login.run () ()
  unsetEnv "LDEBUG"
  unsetEnv "LAMDERA_APP_NAME"


compileCore = do
  withDebug $ do
    let project = "/Users/mario/lamdera/overrides/packages/lamdera/core/1.0.0"
    aggressiveCacheClear project
    Lamdera.Compile.make_ project


compileCodecs =
  withDebug $ do
    let project = "/Users/mario/lamdera/overrides/packages/lamdera/codecs/1.0.0"
    aggressiveCacheClear project
    Lamdera.Compile.make_ project


checkUserConfig = do
  let projectPath = "/Users/mario/lamdera/test/v1"
      appName = "always-v0"

  setEnv "LDEBUG" "1"

  Dir.withCurrentDirectory projectPath $
    do
        Lamdera.Compile.make projectPath "src/Backend.elm"
        Lamdera.Compile.make projectPath "src/Frontend.elm"
        Lamdera.AppConfig.checkUserConfig appName (Just (T.pack adminToken))

  unsetEnv "LDEBUG"
