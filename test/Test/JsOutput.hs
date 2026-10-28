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
    , scope "Wire3: a record holding a Browser.Navigation.Key round trips" $ requireBinaryOnPath "node" $ do
      -- The Key is a JS function at runtime, not a `Key` value. 1.5.0's direct encoder
      -- looked at its `.$`, wrote nothing, and the decode failed: `lamdera live` freeze
      -- mode then reset every FrontendModel holding a Key on reload.
      io $ wire3RoundTrip "lamdera-wire-key" navigationKeyElmJson navigationKeyMain "function () {}"
      -- In Lamdera's own elm/browser (extra/package-replacements) the Key is `{ $: 0 }`, a
      -- password that Browser.Navigation's functions never look into.
      io $ wire3RoundTrip "lamdera-wire-key" navigationKeyElmJson navigationKeyMain "{ $: 0 }"
      ok
    , scope "Wire3: a record holding kernel-backed single-variant values round trips" $ requireBinaryOnPath "node" $ do
      -- Math.Vector3.Vec3 (`type Vec3 = Vec3`, a Float64Array at runtime) and Regex.Regex
      -- (`type Regex = Regex`, a RegExp) hit the same 1.5.0 bug as the Key. lamdera check
      -- lets a Vec3 into ToBackend, ToFrontend and BackendModel, so 1.5.0 dropped those
      -- messages and could not restore such a BackendModel, in production too.
      --
      -- 1.5.1 still decoded every linear-algebra value to a placeholder that lost it. They
      -- now have a real codec (Wire3.Helpers.KernelCodec): the decoded values must be the
      -- ones encoded, and work with the package's functions. Regex, which lamdera check
      -- rejects, now fails to encode instead of round tripping a placeholder.
      io $ wire3RoundTrip "lamdera-wire-kernel" kernelValuesElmJson kernelValuesMain "undefined"
      ok
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


{-| Builds the project (an `elm.json` and a `src/Main.elm` whose `roundTrip` returns
`Maybe Int`) in dev and --optimize, calls `roundTrip` with the JS expression `argument`,
and fails unless it gives `Just 5`.
-}
wire3RoundTrip :: String -> T.Text -> T.Text -> String -> IO ()
wire3RoundTrip name elmJson main argument =
  Temp.withSystemTempDirectory name $ \dir -> do
    writeUtf8 (dir </> "elm.json") elmJson
    writeUtf8 (dir </> "src/Main.elm") main
    environment <- Env.getEnvironment
    forM_ [[], ["--optimize"]] $ \mode -> do
      -- Its own ELM_HOME, so the packages' codecs are built by this compiler
      let child = (Process.proc "lamdera" (["make", "src/Main.elm", "--output=elm.js"] ++ mode))
            { Process.cwd = Just dir
            , Process.env = Just (("ELM_HOME", dir </> "elm-home") : filter (\(key, _) -> key /= "ELM_HOME" && key /= "LDEBUG") environment)
            }
      (exit, out, err) <- Process.readCreateProcessWithExitCode child ""
      when (exit /= ExitSuccess) $ ioError $ userError $ "lamdera make failed:\n" ++ out ++ err
      Just javascript <- readUtf8Text (dir </> "elm.js")
      let (before, after) = T.breakOnEnd "}(this));" javascript
      writeUtf8 (dir </> "elm.js") $
        T.dropEnd 9 before <> "\nscope.roundTrip = $author$project$Main$roundTrip;\n}(this));" <> after
      esmCommand dir "node"
        ["-e", "const r = require('./elm.js').roundTrip(" ++ argument ++ "); if (!(r.a === 5)) { console.error(r); process.exit(1); }"]


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


navigationKeyElmJson :: T.Text
navigationKeyElmJson = [text|
{
    "type": "application",
    "source-directories": ["src"],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "lamdera/codecs": "1.0.0"
        },
        "indirect": {
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": { "direct": {}, "indirect": {} }
}
|]


navigationKeyMain :: T.Text
navigationKeyMain = [text|
module Main exposing (..)

import Browser.Navigation
import Lamdera.Wire3 as W
import Platform


type alias WithKey =
    { key : Browser.Navigation.Key, n : Int }


roundTrip : Browser.Navigation.Key -> Maybe Int
roundTrip key =
    W.bytesEncode (w3_encode_WithKey { key = key, n = 5 })
        |> W.bytesDecode w3_decode_WithKey
        |> Maybe.map .n


main : Program () (Browser.Navigation.Key -> Maybe Int) ()
main =
    Platform.worker
        { init = \_ -> ( roundTrip, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
|]


kernelValuesElmJson :: T.Text
kernelValuesElmJson = [text|
{
    "type": "application",
    "source-directories": ["src"],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "elm/json": "1.1.3",
            "elm/regex": "1.0.0",
            "elm-explorations/linear-algebra": "1.0.3",
            "elm-explorations/webgl": "1.1.3",
            "lamdera/codecs": "1.0.0"
        },
        "indirect": {
            "elm/html": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": { "direct": {}, "indirect": {} }
}
|]


kernelValuesMain :: T.Text
kernelValuesMain = [text|
module Main exposing (..)

import Lamdera.Wire3 as W
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Platform
import Regex exposing (Regex)
import WebGL


type Wrapped a
    = Wrapped a


type alias Kernel =
    { position : Vec3
    , positions : List Vec3
    , wrapped : Wrapped Vec3
    , v2 : Vec2
    , v4 : Vec4
    , m4 : Mat4
    , mesh : WebGL.Mesh { pos : Vec3 }
    , n : Int
    }


type alias WithRegex =
    { regex : Regex, n : Int }


kernel : Kernel
kernel =
    { position = vec3 1 2 3
    , positions = [ vec3 4 5 6 ]
    , wrapped = Wrapped (vec3 7 8 9)
    , v2 = vec2 1.5 (-2.5)
    , v4 = vec4 1 2 3 4
    , m4 = Mat4.makeRotate 0.3 (vec3 1 2 3) |> Mat4.translate3 4 5 6
    , mesh = WebGL.triangles [ ( { pos = vec3 0 0 0 }, { pos = vec3 1 0 0 }, { pos = vec3 0 1 0 } ) ]
    , n = 5
    }


same : Kernel -> Kernel -> Bool
same a b =
    (Vec3.toRecord a.position == Vec3.toRecord b.position)
        && (List.map Vec3.toRecord a.positions == List.map Vec3.toRecord b.positions)
        && (case ( a.wrapped, b.wrapped ) of
                ( Wrapped x, Wrapped y ) ->
                    Vec3.toRecord x == Vec3.toRecord y
           )
        && (Vec2.toRecord a.v2 == Vec2.toRecord b.v2)
        && (Vec4.toRecord a.v4 == Vec4.toRecord b.v4)
        && (Mat4.toRecord a.m4 == Mat4.toRecord b.m4)
        && (a.mesh == b.mesh)
        -- the decoded values are the kernel's own, so its functions work on them
        && (Vec3.getX (Vec3.add b.position (vec3 1 1 1)) == 2)
        && (Mat4.toRecord (Mat4.mul b.m4 Mat4.identity) == Mat4.toRecord a.m4)


roundTrip : () -> Maybe Int
roundTrip _ =
    let
        regexDecoded =
            W.bytesEncode (w3_encode_WithRegex { regex = Regex.fromString "a+" |> Maybe.withDefault Regex.never, n = 5 })
                |> W.bytesDecode w3_decode_WithRegex
    in
    W.bytesEncode (w3_encode_Kernel kernel)
        |> W.bytesDecode w3_decode_Kernel
        |> Maybe.andThen
            (\decoded ->
                if same kernel decoded && regexDecoded == Nothing then
                    Just decoded.n

                else
                    Nothing
            )


main : Program () (() -> Maybe Int) ()
main =
    Platform.worker
        { init = \_ -> ( roundTrip, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
|]
