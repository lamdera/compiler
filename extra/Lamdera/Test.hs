{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Test where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal

import qualified Check

import System.Process (callCommand)
import System.Environment (setEnv, unsetEnv)

import Lamdera

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


-- COMPILE


compile :: IO ()
compile = do
  -- let project = "/Users/mario/lamdera/test/v1"
  -- setEnv "LAMDERA_APP_NAME" "testapp"

  -- let project = "/Users/mario/dev/projects/elm-spa-example"
  -- setEnv "LAMDERA_APP_NAME" "realworldish"

  -- let project = "/Users/mario/dev/projects/lamdera-test"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  -- let project = "/Users/mario/tmp/lamdera-experiments"
  -- setEnv "LAMDERA_APP_NAME" "lamderatest"

  let project = "/Users/mario/dev/projects/lamdera-dashboard"
  setEnv "LAMDERA_APP_NAME" "dashboard"

  -- Bust Elm's caching with this one weird trick!
  -- touch $ project </> "src/Types.elm"

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


tempFileName :: FilePath
tempFileName =
  "/dev/null"


cp :: String -> String -> IO ()
cp = Lamdera.copyFile


rm :: String -> IO ()
rm path = Lamdera.remove path


-- CHECK
check = do
  -- checkWithParams "/Users/mario/lamdera/test/v3" "test-local"
  checkWithParams "/Users/mario/lamdera/test/v2" "test-local"
  -- checkWithParams "/Users/mario/dev/projects/lamdera-dashboard" "dashboard"


checkWithParams projectPath appName = do
  setEnv "LAMDERA_APP_NAME" appName
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
  -- setEnv "NOTPROD" "1"
  -- setEnv "HOIST_REBUILD" "1"
  -- setEnv "VERSION" "3"

  cp "/Users/mario/lamdera/runtime/src/LBR.elm" (projectPath ++ "/src/LBR.elm")
  cp "/Users/mario/lamdera/runtime/src/LFR.elm" (projectPath ++ "/src/LFR.elm")
  cp "/Users/mario/lamdera/runtime/src/RPC.elm" (projectPath ++ "/src/RPC.elm")
  cp "/Users/mario/lamdera/runtime/src/LamderaHelpers.elm" (projectPath ++ "/src/LamderaHelpers.elm")

  Dir.withCurrentDirectory projectPath $
    do
        Check.run () ()

  rm (projectPath ++ "/src/LBR.elm")
  rm (projectPath ++ "/src/LFR.elm")
  rm (projectPath ++ "/src/RPC.elm")
  rm (projectPath ++ "/src/LamderaHelpers.elm")

  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"
  unsetEnv "NOTPROD"



checkSnapshotWithParams version projectPath appName = do
  setEnv "LTYPESNAPSHOT" version
  checkWithParams projectPath appName
  unsetEnv "LTYPESNAPSHOT"



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

        _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        return ()
