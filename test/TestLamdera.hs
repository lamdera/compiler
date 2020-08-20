{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestLamdera where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T

import qualified Elm.Project as Project
import qualified Elm.Project.Summary as Summary
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal

import System.Process (callCommand)
import System.Environment (setEnv, unsetEnv)
import NeatInterpolation
import Test.Main (captureProcessResult, withStdin)
import qualified Data.ByteString.Lazy as BSL
import qualified System.Environment as Env

import Lamdera
import EasyTest
import qualified Init
import qualified Lamdera.Login
import qualified Lamdera.Check
import qualified Lamdera.Secrets
import qualified Lamdera.Update


all = run suite

single = EasyTest.rerunOnly 2641118768625101232 "init should write .gitignore" suite


suite :: Test ()
suite = tests
  [ scope "init should write .gitignore" $
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
              expectTextContains ignore "lamdera-stuff"

            Nothing ->
              crash $ "Expected to find " <> tmpFolder <> "/.gitignore but didn't."

      in
      using setup cleanup test

  , scope "warning about external packages" $ do
      actual <- catchOutput $ checkWithParamsNoDebug 1 "/Users/mario/lamdera/test/v1" "test-local"

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

withStdinYesAll :: IO a -> IO a
withStdinYesAll action =
  -- This doesn't work as `withStdIn` actually writes to a file to emulate stdin
  -- withStdin (BSL.cycle "\n") action

  -- So instead, expect that our CLI will be reasonable and never ask the user to confirm more than this many times...
  withStdin
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    action




{-

This is a modified clone of ui/terminal/src/Develop/StaticFiles/Build.hs
specifically to help with the development cycle for Evergreen.

Changes are:
- Generates `Output.Html` instead of `Output.Javascript` (so we can just refresh browser after run)
- Uses `Output.Dev` instead of `Output.Prod` to avoid errors associated with Debug usage in AllTypes_Check

Here's a suggested development flow to use this:

1. Put the following into ~/.ghci

:set -fbyte-code
:set -fobject-code
:set prompt "\ESC[34mλ: \ESC[m"

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Then a feedback loop goes as follows;

  - Make changes to Haskell Wire code
  - Run `:r` to typecheck + recompile & fix any issues
  - Run `WireTest.compile`
    - Executes shell `touch` on `extra/src/AllTypes.elm` to bust Elm compiler's cache
    - Compiles `extra/src/AllTypes_Check.elm`
    - Generates `extra/src/wire.html`
  - Refresh `wire.html` in your browser – you should see big green boxes
    - If something is red, you broke encoders/decoders!

-}
compile :: IO ()
compile = do
  let project = "/Users/mario/lamdera/test/v1"
  -- setEnv "LAMDERA_APP_NAME" "testapp"
  setEnv "TOKEN" "a739477eb8bd2acbc251c246438906f4"

  -- let project = "/Users/mario/dev/projects/elm-spa-example"
  -- setEnv "LAMDERA_APP_NAME" "realworldish"

  -- let project = "/Users/mario/dev/projects/lamdera-test"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/tmp/lamdera-experiments"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/dev/projects/lamdera-dashboard"
  -- setEnv "LAMDERA_APP_NAME" "dashboard"

  -- Bust Elm's caching with this one weird trick!
  touch $ project </> "src/Frontend.elm"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  let rootPaths = [ "src" </> "Frontend.elm" ]

  Dir.withCurrentDirectory project $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing tempFileName)
              Project.compile Output.Dev Output.Client jsOutput Nothing summary rootPaths

        _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        return ()

  unsetEnv "TOKEN"
  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"


tempFileName :: FilePath
tempFileName =
  "/dev/null"


cp :: String -> String -> IO ()
cp = Lamdera.copyFile


rm :: String -> IO ()
rm path = Lamdera.remove path



{-| For quick and general local development testing via `stack ghci` as TestLamdera.check -}
check = do
  -- checkWithParams "/Users/mario/lamdera/test/v1" "test-local"
  checkWithParams "/Users/mario/dev/test/ascii-art" "ascii-art-local"
  -- checkWithParams "/Users/mario/dev/test/lamdera-minilatex-app" "minilatex"
  -- checkWithParams "/Users/mario/dev/lamdera-user-projects/beat-the-big-two" "beat-the-big-two"
  -- checkWithParams "/Users/mario/dev/projects/lamdera-dashboard" "dashboard"
  -- checkWithParams "/Users/mario/dev/projects/lamdera-mogee" "mogee"
  -- checkWithParams "/Users/mario/lamdera/tmp/elm-audio-test0" "elm-audio-test0"


{-| Run the `lamdera check` pipeline with specific params -}
checkWithParams projectPath appName = do
  setEnv "LAMDERA_APP_NAME" appName
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
  setEnv "NOTPROD" "1"
  setEnv "TOKEN" "a739477eb8bd2acbc251c246438906f4"
  -- setEnv "HOIST_REBUILD" "1"
  -- setEnv "VERSION" "1"

  cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  -- cp "/Users/mario/lamdera/runtime/src/RPC.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  Dir.withCurrentDirectory projectPath $
    do
        Lamdera.Check.run () ()

  rm (projectPath ++ "/src/LBR.elm")
  rm (projectPath ++ "/src/LFR.elm")
  -- rm (projectPath ++ "/src/RPC.elm")
  rm (projectPath ++ "/src/LamderaHelpers.elm")

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

  Dir.withCurrentDirectory projectPath $ Lamdera.Check.run () ()

  rm (projectPath ++ "/src/LBR.elm")
  rm (projectPath ++ "/src/LFR.elm")
  rm (projectPath ++ "/src/RPC.elm")
  rm (projectPath ++ "/src/LamderaHelpers.elm")

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "VERSION"
  unsetEnv "LOVR"
  unsetEnv "ELM_HOME"
  unsetEnv "NOTPROD"


{-| Run the type snapshot part of `lamdera check` only, with specific params -}
snapshotWithParams :: Int -> FilePath -> String -> IO ()
snapshotWithParams version projectPath appName = do
  setEnv "LTYPESNAPSHOT" (show version)
  setEnv "LAMDERA_APP_NAME" appName
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  let rootPaths = [ "src" </> "Types.elm" ]

  Dir.withCurrentDirectory projectPath $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  summary <- Project.getRoot
              let root = Summary._root summary

              liftIO $ Lamdera.touch $ root </> "src" </> "Types.elm"

              let jsOutput = Just (Output.Html Nothing tempFileName)
              Project.compile Output.Dev Output.Client jsOutput Nothing summary rootPaths


  -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
  liftIO $ sleep 50 -- 50 milliseconds

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"
  unsetEnv "LTYPESNAPSHOT"


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

  Dir.withCurrentDirectory project $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing tempFileName)
              Project.compile Output.Dev Output.Client jsOutput Nothing summary rootPaths

        -- _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        return ()


config = do
  -- setEnv "LDEBUG" "1"
  -- let project = "/Users/mario/lamdera/test/v1"
  let project = "/Users/mario/dev/projects/lamdera-test"
  Dir.withCurrentDirectory project $ do
    reporter <- Terminal.create
    setEnv "TOKEN" "a739477eb8bd2acbc251c246438906f4"

    prodTokenM <- Env.lookupEnv "TOKEN"
    Task.run reporter $ do
      Lamdera.Secrets.checkUserConfig "test-local" (fmap T.pack prodTokenM)

    unsetEnv "TOKEN"
    unsetEnv "LDEBUG"


http = do
  reporter <- Terminal.create
  Task.run reporter $ do
    res <- Lamdera.Update.fetchCurrentVersion
    liftIO $ putStrLn $ show res


login = do
  let project = "/Users/mario/lamdera/test/v1"
  setEnv "LDEBUG" "1"
  -- setEnv "LAMDERA_APP_NAME" "test-local"
  Dir.withCurrentDirectory project $
    Lamdera.Login.run () ()
  unsetEnv "LDEBUG"
  unsetEnv "LAMDERA_APP_NAME"
