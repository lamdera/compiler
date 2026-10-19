{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.HotPatch where

{-| Lamdera.HotPatch, the `lamdera live` hot patches.

A small Platform.worker app is built in a live session after each edit, once with the
artifacts only in memory (`lamdera live --memory`) and once on disk (plain `lamdera
live`). The patch from the
build before must be a patch where one is safe (and only what it needs), and a full
reload where it is not. And in node, running the first build's JS and then each patch
in its scope must answer like the latest build does.
-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NonEmptyList as NE
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import qualified System.Process
import NeatInterpolation

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified Build
import qualified Elm.Details as Details
import qualified Ext.Common
import qualified Ext.MemoryCache as MemoryCache
import qualified Generate
import qualified Lamdera.CLI.Live
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R


all = EasyTest.run suite


suite :: Test ()
suite =
  tests
    [ scope "InMemory" $ suiteFor MemoryCache.InMemory
    , scope "OnDisk" $ suiteFor MemoryCache.OnDisk
    ]


suiteFor :: MemoryCache.Backing -> Test ()
suiteFor backing =
  let
    setup = do
      dir <- Dir.makeAbsolute ("./test/scenario-hot-patch-" ++ show backing)
      writeProject dir
      stuff <- R.getStuff (R.fromFilePath dir)
      store <- MemoryCache.register backing stuff
      pure (dir, store)

    cleanup (dir, store) = do
      MemoryCache.unregister store
      rmdir dir

    test (dir, store) =
      let
        -- edit, and tell the store, as the file watcher would
        edit path f = io $ do
          editFile (dir </> path) f
          _ <- MemoryCache.sourcesChanged store [dir </> path]
          pure ()

        -- build, keep the JS as build-<n>.js, and return n
        build = do
          result <- io $ buildJs dir
          case result of
            Left problem -> crash problem
            Right js -> do
              version <- io $ MemoryCache.jsVersion store
              io $ BS.writeFile (dir </> ("build-" ++ show version ++ ".js")) js
              pure version

        -- the patch from build `from` to the latest, kept as patch-<from>.js
        patchFrom from = do
          patch <- io $ Lamdera.CLI.Live.hotPatch (Just store) from
          io $ BS.writeFile (dir </> ("patch-" ++ show from ++ ".js")) patch
          pure patch

        -- run build `start` in node, then the given patches, asking after each
        answers start patches = do
          (exit, out, err) <- io $ System.Process.readProcessWithExitCode "node"
            ((dir </> "run.js") : (dir </> ("build-" ++ show start ++ ".js")) : fmap (\p -> dir </> ("patch-" ++ show (p :: Int) ++ ".js")) patches) ""
          case exit of
            ExitSuccess -> pure (T.strip (T.pack out))
            ExitFailure _ -> crash ("node failed:\n" ++ err)

        isPatch = BS.isPrefixOf "patch "
        isFull = BS.isPrefixOf "full "
        has name patch = BS.isInfixOf (name <> " = ") patch
      in
      do  v1 <- build
          expectEqual "[\"q LEAF-V12\"]" =<< answers v1 []

          scope "a changed value is patched, with the values that use it worked out again" $ do
            edit "src/Leaf.elm" (T.replace "\"leaf-v1\"" "\"leaf-v2\"")
            _ <- build
            patch <- patchFrom v1
            expect (isPatch patch)
            expect (has "$author$project$Leaf$base" patch)
            expect (has "$author$project$Leaf$suffix" patch)
            expect (has "$author$project$Main$main" patch)
            -- functions see the new values when called, so are left alone
            expect (not (has "$author$project$Leaf$reply" patch))
            expect (not (has "$author$project$Leaf$unrelated" patch))
            expectEqual "[\"q LEAF-V12\",\"q LEAF-V22\"]" =<< answers v1 [v1]

          v2 <- io $ MemoryCache.jsVersion store
          scope "a changed function is patched, and a new definition defined" $ do
            edit "src/Leaf.elm" (T.replace "q ++ \" \" ++ suffix" "q ++ \" | \" ++ suffix ++ extra")
            edit "src/Leaf.elm" (<> "\n\nextra : String\nextra =\n    \"!\"\n")
            _ <- build
            patch <- patchFrom v2
            expect (isPatch patch)
            expect (has "$author$project$Leaf$reply" patch)
            expect (has "$author$project$Leaf$extra" patch)
            expect (not (has "$author$project$Leaf$base" patch))
            expectEqual "[\"q LEAF-V12\",\"q LEAF-V22\",\"q | LEAF-V2!2\"]" =<< answers v1 [v1, v2]

          v3 <- io $ MemoryCache.jsVersion store
          scope "patches from an older build run like one" $ do
            _ <- patchFrom v1
            expectEqual "[\"q LEAF-V12\",\"q | LEAF-V2!2\"]" =<< answers v1 [v1]

          scope "a build with nothing new is an empty patch" $ do
            _ <- build
            patch <- patchFrom v3
            expectEqual (BS8.pack ("patch " ++ show (v3 + 1) ++ "\n")) patch

          v4 <- io $ MemoryCache.jsVersion store
          scope "a new port is a full reload" $ do
            edit "src/Main.elm" (T.replace "port answer : String -> Cmd msg" "port answer : String -> Cmd msg\n\n\nport other : String -> Cmd msg")
            edit "src/Main.elm" (T.replace "answer (Leaf.reply q" "Cmd.batch [ other \"\", answer (Leaf.reply q")
            edit "src/Main.elm" (T.replace "(Leaf.unrelated 1)) )" "(Leaf.unrelated 1)) ] )")
            _ <- build
            patch <- patchFrom v4
            expect (isFull patch)

          v5 <- io $ MemoryCache.jsVersion store
          scope "new kernel code is a full reload" $ do
            edit "src/Leaf.elm" (T.replace "n + 1" "Bitwise.and n 255 + 1")
            edit "src/Leaf.elm" (T.replace "module Leaf exposing (reply, unrelated)" "module Leaf exposing (reply, unrelated)\n\nimport Bitwise")
            _ <- build
            patch <- patchFrom v5
            expect (isFull patch)

          scope "a build no longer kept is a full reload" $ do
            patch <- patchFrom 0
            expect (isFull patch)
  in
  using setup cleanup test


{-| What Develop.compile does for `lamdera live` (without the live harness). -}
buildJs :: FilePath -> IO (Either String BS.ByteString)
buildJs dir = do
  result <-
    Ext.Common.withProjectRoot dir $
    R.withRootLock (R.fromFilePath dir) $ \writer stuff ->
      Task.run $
        do  let root = R.fromFilePath dir
            details <- Task.eio Exit.ReactorBadDetails $ Details.load writer Reporting.silent root stuff
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths writer Reporting.silent root stuff details (NE.List (dir </> "src/Main.elm") [])
            Task.mapError Exit.ReactorBadGenerate $ Generate.dev stuff details artifacts
  pure $
    case result of
      Right javascript -> Right (BSL.toStrict (B.toLazyByteString javascript))
      Left exit -> Left (Exit.toString (Exit.reactorToReport exit))



-- PROJECT


editFile :: FilePath -> (T.Text -> T.Text) -> IO ()
editFile path f = do
  before <- readUtf8Text path
  case before of
    Just text -> do
      -- mtimes are compared for equality, but make sure a fast edit still moves it
      sleep 10
      writeUtf8 path (f text)
    Nothing ->
      error $ "editFile: cannot read " ++ path


writeProject :: FilePath -> IO ()
writeProject dir = do
  rmdir dir
  Dir.createDirectoryIfMissing True (dir </> "src")
  writeUtf8 (dir </> "elm.json") elmJson
  writeUtf8 (dir </> "src/Main.elm") mainModule
  writeUtf8 (dir </> "src/Leaf.elm") leafModule
  writeUtf8 (dir </> "run.js") runJs


elmJson :: T.Text
elmJson = [text|
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.3",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            "elm/json": "1.1.4"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
|]


mainModule :: T.Text
mainModule = [text|
port module Main exposing (main)

import Leaf


port ask : (String -> msg) -> Sub msg


port answer : String -> Cmd msg


type Msg
    = Asked String


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \(Asked q) model -> ( model, answer (Leaf.reply q ++ String.fromInt (Leaf.unrelated 1)) )
        , subscriptions = \_ -> ask Asked
        }
|]


leafModule :: T.Text
leafModule = [text|
module Leaf exposing (reply, unrelated)


reply : String -> String
reply q =
    q ++ " " ++ suffix


suffix : String
suffix =
    String.toUpper base


base : String
base =
    "leaf-v1"


unrelated : Int -> Int
unrelated n =
    n + 1
|]


-- node run.js <build> <patch>...: start the build, then run each patch in its scope (as
-- extra/live.js does), asking after each, and print the answers
runJs :: T.Text
runJs = [text|
const fs = require('fs')
const [bundle, ...patches] = process.argv.slice(2)
const scope = {}
new Function(fs.readFileSync(bundle, 'utf8')).call(scope)
const app = scope.Elm.Main.init()
const answers = []
app.ports.answer.subscribe(a => answers.push(a))
const tick = () => new Promise(resolve => setTimeout(resolve, 0))
;(async () => {
  app.ports.ask.send('q')
  await tick()
  for (const path of patches) {
    const text = fs.readFileSync(path, 'utf8')
    if (!text.startsWith('patch ')) throw new Error('not a patch: ' + text.slice(0, 200))
    const code = text.slice(text.indexOf('\n') + 1)
    if (code.length > 0) scope.__lamdera_patch(code)
    await tick()
    app.ports.ask.send('q')
    await tick()
  }
  console.log(JSON.stringify(answers))
})().catch(e => { console.error(e); process.exit(1) })
|]
