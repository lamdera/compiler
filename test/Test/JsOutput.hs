{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.JsOutput where

import Lamdera
import EasyTest
import Test.Helpers

import qualified Init
import Make (Flags(..))
import qualified Make
import qualified Ext.Common
import qualified Lamdera.Relative
import qualified Test.HotPatch as HotPatch
import qualified Data.Text as T
import Control.Monad (forM_, when)
import NeatInterpolation (text)
import qualified System.Environment as Env
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.Process as Process
import qualified System.IO.Temp as Temp

suite :: Test ()
suite =
  tests
    [ scope "a tail-call function will not re-assign values to itself unnecessarily" $ do
      project <- io $ Lamdera.Relative.requireDir "test/generated-javascript"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
        rmdir elmHome
        rmdir elmStuff

        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = True
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$temp$list = xs"
            expectTextDoesNotContain jsOutput "$temp$fn = fn"

        Nothing ->
          crash "JS output could not be read."
    , scope "direct function calls (lamdera/compiler PR #41)" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
        rmdir elmHome
        rmdir elmStuff

        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = True
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$fn2$ = function (x, y) {"
            expectTextContains jsOutput "$fn3$ = function (x, y, z) {"
            expectTextContains jsOutput "$Ctor2$ = function (a, b) {"
            expectTextContains jsOutput "$Ctor3$ = function (a, b, c) {"
            expectTextContains jsOutput "$N2$ = function (a, b) {"
            expectTextContains jsOutput "x1 = $author$project$Main$fn1(0)"
            expectTextContains jsOutput "x2 = $author$project$Main$fn2$(0, 0)"
            expectTextContains jsOutput "x3 = $author$project$Main$fn3$(0, 0, 0)"
            expectTextContains jsOutput "c1 = $author$project$Main$Ctor1("
            expectTextContains jsOutput "c2 = $author$project$Main$Ctor2$("
            expectTextContains jsOutput "c3 = $author$project$Main$Ctor3$("
            expectTextContains jsOutput "n1 = 0"
            expectTextContains jsOutput "n2 = $author$project$Main$N2$(0, 0)"

        Nothing ->
          crash "JS output could not be read."
    , scope "direct function calls - mutual recursion" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls-mutual-recursion"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
        rmdir elmHome
        rmdir elmStuff

        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = True
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$Main$a2 = function (n) {"
            expectTextContains jsOutput "$Main$cyclic$a1() {"
            expectTextContains jsOutput "$Main$a1 ="
            expectTextContains jsOutput "$Main$cyclic$a1 = function () {"
            expectTextContains jsOutput "$Main$a1(1)"
            expectTextContains jsOutput "$Main$a2(1)"

            expectTextContains jsOutput "$Main$b2$ = function (m, n) {"
            expectTextContains jsOutput "$Main$cyclic$b1()"
            expectTextContains jsOutput "$Main$b2 = F2("
            expectTextContains jsOutput "$Main$cyclic$b1 = "
            expectTextContains jsOutput "$Main$b1 ="
            expectTextContains jsOutput "$Main$cyclic$b1 = function () {"
            expectTextContains jsOutput "$Main$b1, 1, 1)" -- A2
            expectTextContains jsOutput "$Main$b2$(1, 1)"

        Nothing ->
          crash "JS output could not be read."
    , scope "direct function calls - mutual recursion with partial application" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls-mutual-recursion-partial-application"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
        rmdir elmHome
        rmdir elmStuff

        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = True
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$Main$a2$ = function (x1, x2, x3, x4, x5) {"
            expectTextContains jsOutput "$Main$a2 = F5"
            expectTextContains jsOutput "$Main$cyclic$a1() {"
            expectTextContains jsOutput "$Main$a2, 1, 2);"
            expectTextContains jsOutput "$Main$a1 ="
            expectTextContains jsOutput "$Main$cyclic$a1 = function () {"
            expectTextContains jsOutput "$Main$a1, 3, 4, 5)" -- A3
            expectTextContains jsOutput "$Main$a2$(1, 2, 3, 4, 5)"

        Nothing ->
          crash "JS output could not be read."
    , scope "Bytes in ports and flags without lamdera/codecs is a compile error, not a crash (lamdera/compiler#26, #59)" $ do
      project <- io $ Lamdera.Relative.requireDir "test/bytes-ports-without-codecs"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      io $ rmdir elmHome >> rmdir elmStuff
      actual <- catchOutput $
        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Generate.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = False
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                , _esm = False
                }
      io $ rmdir elmHome >> rmdir elmStuff

      expectTextContains actual "port `inc` in Generate"
      expectTextContains actual "port `out` in Generate"
      expectTextContains actual "the flags of `main` in Generate"
      expectTextContains actual "lamdera install lamdera/codecs"
      expectTextDoesNotContain actual "unable to find"
    , scope "ESM named export and stateful hot reload" $ requireBinaryOnPath "node" $ esmProject $ \dir -> do
      writeUtf8 (dir </> "check.mjs") hotReloadCheck
      forM_ [[], ["--debug"]] $ \mode -> do
        HotPatch.editFile (dir </> "src/Leaf.elm") (T.replace "leaf-v2" "leaf-v1")
        esmMake dir (mode ++ ["--esm", "--output=Main.mjs"])
        HotPatch.editFile (dir </> "src/Leaf.elm") (T.replace "leaf-v1" "leaf-v2")
        esmMake dir (mode ++ ["--esm", "--output=Changed.mjs"])
        esmCommand dir "node" ["check.mjs"]
      esmMake dir ["--esm", "--optimize", "--output=Main.mjs"]
      esmCommand dir "node"
        ["--input-type=module", "-e", "import { Elm } from './Main.mjs'; import assert from 'node:assert/strict'; assert.equal(Elm.hot, undefined); assert.ok(Elm.Main.init().ports.ask);"]
    , scope "ESM rejects explicit and default HTML output" $ esmProject $ \dir -> do
      forM_ [[], ["--debug"], ["--optimize"], ["--optimize-legible"]] $ \mode ->
        forM_ [[], ["--output=index.html"]] $ \output -> do
          let child = (Process.proc "lamdera" (["make", "src/Main.elm", "--no-wire", "--esm"] ++ mode ++ output))
                { Process.cwd = Just dir }
          (exit, out, err) <- Process.readCreateProcessWithExitCode child ""
          when (exit == ExitSuccess || not ("ESM REQUIRES JAVASCRIPT OUTPUT" `T.isInfixOf` T.pack (out ++ err))) $
            ioError $ userError $ "Expected ESM/HTML rejection:\n" ++ out ++ err
          exists <- Dir.doesFileExist (dir </> "index.html")
          when exists $ ioError $ userError "Rejected ESM/HTML compilation wrote index.html"
      esmMake dir ["--output=index.html"]
      esmMake dir ["--esm", "--output=/dev/null"]
      Just source <- readUtf8Text (dir </> "src/Main.elm")
      writeUtf8 (dir </> "src/Other.elm") (T.replace "port module Main" "port module Other" source)
      esmCommand dir "lamdera" ["make", "src/Main.elm", "src/Other.elm", "--no-wire", "--esm"]
      Just javascript <- readUtf8Text (dir </> "elm.js")
      when (not ("export const Elm" `T.isInfixOf` javascript)) $
        ioError $ userError "Multiple ESM mains without --output must generate JavaScript"
    ]


esmProject :: (FilePath -> IO ()) -> Test ()
esmProject check = do
  io $ Temp.withSystemTempDirectory "lamdera-esm" $ \dir -> do
    HotPatch.writeProject dir
    -- A nontrivial model makes resetting the worker on reload observable.
    HotPatch.editFile (dir </> "src/Main.elm") $
      T.replace "Program () () Msg" "Program () Int Msg"
      . T.replace "( (), Cmd.none )" "( 0, Cmd.none )"
      . T.replace "( model, answer" "( model + 1, answer"
      . T.replace "(Leaf.unrelated 1)" "(model + 1)"
    check dir
  ok


-- Use the CLI in a fresh process: ESM and LDEBUG are sticky process globals.
esmMake :: FilePath -> [String] -> IO ()
esmMake dir flags = esmCommand dir "lamdera" (["make", "src/Main.elm", "--no-wire"] ++ flags)


esmCommand :: FilePath -> FilePath -> [String] -> IO ()
esmCommand dir binary args = do
  environment <- Env.getEnvironment
  let child = (Process.proc binary args)
        { Process.cwd = Just dir
        , Process.env = Just (("LDEBUG", "1") : filter (\(key, _) -> key /= "LDEBUG") environment)
        }
  (exit, out, err) <- Process.readCreateProcessWithExitCode child ""
  case exit of
    ExitSuccess -> pure ()
    ExitFailure code -> ioError $ userError $ unwords (binary : args) ++ " failed (" ++ show code ++ "):\n" ++ out ++ err


hotReloadCheck :: T.Text
hotReloadCheck = [text|
import assert from 'node:assert/strict';
import { Elm } from './Main.mjs';
import * as changed from './Changed.mjs';
assert.equal(typeof Elm.hot?.reload, 'function');
const app = Elm.Main.init();
const answers = [];
app.ports.answer.subscribe(value => answers.push(value));
const tick = () => new Promise(resolve => setTimeout(resolve, 10));
app.ports.ask.send('q');
await tick();
assert.deepEqual(answers, ['q LEAF-V11']);
Elm.hot.reload(changed);
app.ports.ask.send('q');
await tick();
assert.deepEqual(answers, ['q LEAF-V11', 'q LEAF-V22']);
app.stop();
|]
