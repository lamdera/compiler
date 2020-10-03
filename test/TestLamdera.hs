{-# LANGUAGE OverloadedStrings #-}

module TestLamdera where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T
import System.Environment (setEnv, unsetEnv, lookupEnv)

import Lamdera
import EasyTest
import qualified Init
import qualified Lamdera.Login
import qualified Lamdera.Check
import qualified Lamdera.AppConfig
import qualified Lamdera.Update
import qualified Lamdera.Compile
import qualified Lamdera.Snapshot
import TestHelp


import Test.Main (captureProcessResult, withStdin)

-- Current target for ghci :rr command. See ~/.ghci config file, which should contain
-- something like `:def rr const $ return $ unlines [":r","TestLamdera.target"]`
target = wire

wire :: IO ()
wire = do
  let project = "/Users/mario/dev/projects/elmx/test"

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  let testFiles =
        -- [ "src/Test/Wire_Union_1_Basic.elm",
        -- , "src/Test/Wire_Union_2_Basic.elm"
        [ "src/Test/Wire_Union_3_Params.elm"
        ]

  testFiles & mapM (\filename -> do
      -- Bust Elm's caching with this one weird trick!
      touch $ project </> filename
      Lamdera.Compile.make project (project </> filename)
    )

  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"


{-

1. Put the following into ~/.ghci

:set -fbyte-code
:set -fobject-code
:def rr const $ return $ unlines [":r","TestLamdera.target"]
:set prompt "\ESC[34mλ: \ESC[m"

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Then a feedback loop goes as follows;

  - Make changes to Haskell Wire code
  - Run `:rr` to recompile + typecheck and auto-run TestLamdera.target
  - fix any issues, then :rr again
  - if you want to recompile without running, do :r

Easier to change the target definition than constantly adjust the :def!

Press up arrow to get history of prior commands.

-}

all = run suite

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
              expectTextContains ignore "lamdera-stuff"

            Nothing ->
              crash $ "Expected to find " <> tmpFolder <> "/.gitignore but didn't."

      in
      using setup cleanup test

  , pending $ scope "warning about external packages" $ do
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


cp :: String -> String -> IO ()
cp = Lamdera.copyFile


rm :: String -> IO ()
rm path = Lamdera.remove path



{-| For quick and general local development testing via `stack ghci` as TestLamdera.check -}
check = do
  touch "/Users/mario/lamdera/test/v1/src/WireTypes.elm"
  checkWithParams "/Users/mario/lamdera/test/v1" "test-local"
  -- checkWithParams "/Users/mario/dev/test/ascii-art" "ascii-art-local"
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
  -- setEnv "NOTPROD" "1"
  setEnv "TOKEN" "a739477eb8bd2acbc251c246438906f4"
  -- setEnv "HOIST_REBUILD" "1"
  -- setEnv "VERSION" "1"

  -- cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  -- cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  -- cp "/Users/mario/lamdera/runtime/src/RPC.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  Dir.withCurrentDirectory projectPath $
    do
        Lamdera.Check.run () ()

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

  Dir.withCurrentDirectory projectPath $ do
    Lamdera.Compile.make projectPath ("src" </> "Types.elm")
    Lamdera.Snapshot.run

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

  Lamdera.Compile.make project ("src" </> "Frontend.elm")


config = do
  -- setEnv "LDEBUG" "1"
  let project = "/Users/mario/lamdera/test/v1"
  -- let project = "/Users/mario/dev/projects/lamdera-test"
  Dir.withCurrentDirectory project $ do
    setEnv "TOKEN" "a739477eb8bd2acbc251c246438906f4"

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
    Lamdera.Login.run () ()
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
