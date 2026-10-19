{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.LiveMemory where

{-| The `lamdera live` session (Ext.MemoryCache): the build artifacts of a project are kept
as values between rebuilds, either only in memory (`lamdera live --memory`, InMemory) or
on disk as always and in memory too (plain `lamdera live`, OnDisk).

Three copies of the same Lamdera app are built side by side the way `lamdera live`
builds them (the Lamdera.Live harness as the root, Wire3 on, live mode on): one with an
InMemory store registered, one with an OnDisk store, one from disk with no store, as
`lamdera make` builds. The same edits are made to all three. After every edit all must
give byte identical JS (but for the hot patch hook the two sessions add), and all must
have compiled exactly the same modules, which must be the ones the edit should
recompile. The OnDisk copy is also changed behind its back, as another process would
(a build of its own, a touched artifact, elm-stuff removed): it must notice, give the
same JS, and give pages a full reload. At the end the memory copy is flushed to
elm-stuff, and a disk build of it must find nothing to compile but the harness.

-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))
import NeatInterpolation

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified AST.Prim.Module as Module
import qualified Build
import qualified Elm.Details as Details
import qualified Ext.Common
import qualified Ext.MemoryCache as MemoryCache
import qualified Generate
import qualified Generate.JavaScript as JS
import qualified Lamdera.CLI.Live
import qualified Lamdera.PostCompile
import qualified Lamdera.TypeHash
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R


all = EasyTest.run suite


suite :: Test ()
suite =
  let
    setup = do
      memDir <- Dir.makeAbsolute "./test/scenario-live-memory-mem"
      diskDir <- Dir.makeAbsolute "./test/scenario-live-memory-disk"
      sessDir <- Dir.makeAbsolute "./test/scenario-live-memory-session"
      mapM_ writeProject [memDir, diskDir, sessDir]
      Lamdera.setLiveMode True
      store <- registerStore MemoryCache.InMemory memDir
      sess <- registerStore MemoryCache.OnDisk sessDir
      current <- newIORef store
      pure (memDir, diskDir, sessDir, store, sess, current)

    cleanup (memDir, diskDir, sessDir, _, sess, current) = do
      MemoryCache.unregister =<< readIORef current
      MemoryCache.unregister sess
      Lamdera.setLiveMode False
      rmdir memDir
      rmdir diskDir
      rmdir sessDir

    test (memDir, diskDir, sessDir, store, sess, current) =
      let
        -- edit every copy, and tell the stores, as the file watcher would
        edit path f = io $ do
          mapM_ (\dir -> editFile (dir </> path) f) [memDir, diskDir, sessDir]
          s <- readIORef current
          _ <- MemoryCache.sourcesChanged s [memDir </> path]
          _ <- MemoryCache.sourcesChanged sess [sessDir </> path]
          pure ()

        -- edit every copy, and tell nobody
        editUnreported path f = io $ mapM_ (\dir -> editFile (dir </> path) f) [memDir, diskDir, sessDir]

        -- build all three, expect the same JS and the same compiled modules, and that those are `expected`
        step label expected = scope label $ do
          (mem, disk, onDisk) <- io $ (,,) <$> compileLive memDir <*> compileLive diskDir <*> compileLive sessDir
          case (mem, disk, onDisk) of
            (Right (memJs, memFresh), Right (diskJs, diskFresh), Right (sessJs, sessFresh)) -> do
              expectEqual (Set.fromList expected) memFresh
              expectEqual diskFresh memFresh
              expectEqual diskFresh sessFresh
              -- not expectEqual: a failure would print the whole JS. In a session the JS also
              -- has the hot patch hook, and nothing else
              expect (BS.isInfixOf hook memJs && withoutHook memJs == diskJs)
              expect (sessJs == memJs)
              pure memJs

            (Left memErr, Left diskErr, Left _) ->
              crash $ "the builds failed:\n" ++ memErr ++ "\n" ++ diskErr

            _ ->
              crash $ "only some builds failed:\n" ++ show (fmap snd mem) ++ "\n" ++ show (fmap snd disk) ++ "\n" ++ show (fmap snd onDisk)

        -- build the OnDisk copy alone, and compare its JS with the last of the disk copy
        sessionAlone = do
          sessResult <- io $ compileLive sessDir
          diskResult <- io $ compileLive diskDir
          case (sessResult, diskResult) of
            (Right (sessJs, sessFresh), Right (diskJs, _)) -> do
              expect (BS.isInfixOf hook sessJs && withoutHook sessJs == diskJs)
              pure sessFresh
            _ ->
              crash "a build failed"
      in
      do  _ <- step "the first build compiles everything" ["Backend", "Env", "Frontend", "Leaf", "Middle", "Types", "Lamdera.Live", "Lamdera.Repl"]

          _ <- step "a build with no changes compiles only the harness" ["Lamdera.Live"]

          edit "src/Leaf.elm" (T.replace "leaf-v1" "leaf-v2")
          js <- step "a body change to a leaf recompiles only the leaf" ["Leaf", "Lamdera.Live"]
          expect (BS.isInfixOf "leaf-v2" js)

          edit "src/Leaf.elm" (<> "\n\nleafExtra : Int\nleafExtra =\n    1\n")
          _ <- step "an interface change to a leaf recompiles its importer, whose interface stays" ["Leaf", "Middle", "Lamdera.Live"]

          -- Same type, so the same interface and Middle is not recompiled, but leafJoin now
          -- takes one argument: Middle's direct call to it must change in the JS. In memory
          -- the JS of Middle.middleText is reused only while the arities it calls are the same.
          edit "src/Leaf.elm" (T.replace "leafJoin a b =\n    a ++ b" "leafJoin a =\n    \\b -> a ++ b")
          js1 <- step "an arity change behind an unchanged interface regenerates the callers' JS" ["Leaf", "Lamdera.Live"]
          edit "src/Leaf.elm" (T.replace "leafJoin a =\n    \\b -> a ++ b" "leafJoin a b =\n    a ++ b")
          js2 <- step "and back" ["Leaf", "Lamdera.Live"]
          expect (js1 /= js2)


          edit "src/Types.elm" (T.replace "= Red\n    | Green" "= Red\n    | Green\n    | Blue")
          _ <- io $ MemoryCache.takeCounters store
          js2 <- step "a new constructor in Types recompiles its importers, with new Wire3 codecs" ["Types", "Frontend", "Backend", "Lamdera.Live"]
          expect (BS.isInfixOf "$author$project$Types$Blue" js2)
          scope "the importers recompiled for it are not parsed again" $ do
            counters <- io $ MemoryCache.takeCounters store
            expect (MemoryCache._parseHits counters >= 2)
          scope "the type hashes taken from the build are those of a separate build of Types" $ do
            hashes <- io $ typeHashes memDir
            case hashes of
              Right (fromBuild, separate) -> do
                expect (not (null fromBuild))
                expectEqual separate fromBuild
              Left problem ->
                crash problem

          scope "a type error fails both builds" $ do
            edit "src/Middle.elm" (T.replace "middleText =" "middleText =\n    1 + \"x\"\n\n\nbroken =")
            mem <- io $ compileLive memDir
            disk <- io $ compileLive diskDir
            onDisk <- io $ compileLive sessDir
            expect (isLeft mem && isLeft disk && isLeft onDisk)

          edit "src/Middle.elm" (T.replace "middleText =\n    1 + \"x\"\n\n\nbroken =" "middleText =")
          _ <- step "fixing it recompiles only the fixed module" ["Middle", "Lamdera.Live"]

          scope "an edit the watcher did not report is not seen until a reconcile" $ do
            editUnreported "src/Leaf.elm" (T.replace "leaf-v2" "leaf-v2b")
            mem <- io $ compileLive memDir
            expectEqual (Right (Set.fromList ["Lamdera.Live"])) (fmap snd mem)
            changed <- io $ MemoryCache.reconcileSources store
            expectEqual [memDir </> "src/Leaf.elm"] changed
          _ <- step "after the reconcile both see it" ["Leaf", "Lamdera.Live"]

          scope "a directory event forgets every kept source" $ do
            _ <- io $ MemoryCache.takeCounters store
            forgot <- io $ MemoryCache.sourcesChanged store [memDir </> "src"]
            expect forgot
          _ <- step "and the next build looks at the disk again, finding nothing changed" ["Lamdera.Live"]
          scope "(looked at the disk)" $ do
            counters <- io $ MemoryCache.takeCounters store
            expect (MemoryCache._sourceFs counters > 0)

          scope "elm-stuff is not written while the store is registered" $ do
            elmos <- io $ filter (List.isSuffixOf ".elmo") <$> (Dir.listDirectory =<< stuffDir memDir)
            expectEqual [] elmos

          scope "OnDisk elm-stuff is written as always" $ do
            elmos <- io $ filter (List.isSuffixOf ".elmo") <$> (Dir.listDirectory =<< stuffDir sessDir)
            expect (length elmos >= 6)

          scope "OnDisk a rebuild with nothing changed reads nothing from disk" $ do
            _ <- io $ MemoryCache.takeCounters sess
            _ <- sessionAlone
            counters <- io $ MemoryCache.takeCounters sess
            expectEqual 0 (MemoryCache._diskReads counters)
            expect (MemoryCache._objectHits counters > 0)

          scope "OnDisk the type hashes taken from the build are those of a separate build of Types" $ do
            hashes <- io $ typeHashes sessDir
            case hashes of
              Right (fromBuild, separate) -> do
                expect (not (null fromBuild))
                expectEqual separate fromBuild
              Left problem ->
                crash problem

          scope "OnDisk another process building the project is noticed" $ do
            before <- io $ MemoryCache.externalChanges sess
            version <- io $ MemoryCache.jsVersion sess
            edit "src/Leaf.elm" (T.replace "leaf-v2b" "leaf-v2c")
            -- what a `lamdera make` in another terminal would do: build it, write elm-stuff
            other <- io $ MemoryCache.suspended sess (compileLive sessDir)
            expectEqual (Right (Set.fromList ["Leaf", "Lamdera.Live"])) (fmap snd other)
            -- the session's next build: d.dat moved, so everything kept is dropped and it
            -- starts from the disk, finding Leaf compiled already
            fresh <- sessionAlone
            expectEqual (Set.fromList ["Lamdera.Live"]) fresh
            after <- io $ MemoryCache.externalChanges sess
            expect (after > before)
            -- and the page running the build before gets a full reload, not a patch
            patch <- io $ Lamdera.CLI.Live.hotPatch (Just sess) version
            expect (BS.isPrefixOf "full " patch)
          _ <- io $ compileLive memDir -- (catch up: sessionAlone built the disk copy, not this one)
          _ <- step "and every copy agrees after it" ["Lamdera.Live"]

          scope "OnDisk an artifact rewritten behind its back is read again" $ do
            leafElmo <- (</> "Leaf.elmo") <$> io (stuffDir sessDir)
            io $ sleep 10 >> BS.readFile leafElmo >>= \bytes -> BS.length bytes `seq` BS.writeFile leafElmo bytes
            _ <- io $ MemoryCache.takeCounters sess
            before <- io $ MemoryCache.externalChanges sess
            _ <- sessionAlone
            counters <- io $ MemoryCache.takeCounters sess
            expectEqual 1 (MemoryCache._staleReads counters)
            after <- io $ MemoryCache.externalChanges sess
            expectEqual (before + 1) after

          scope "OnDisk elm-stuff removed: the next build starts from scratch" $ do
            io $ rmdir =<< stuffDir sessDir
            fresh <- sessionAlone
            expectEqual (Set.fromList ["Backend", "Env", "Frontend", "Leaf", "Middle", "Types", "Lamdera.Live", "Lamdera.Repl"]) fresh

          io $ mapM_ (\dir -> touchElmJson (dir </> "elm.json")) [memDir, diskDir, sessDir]
          _ <- step "touching elm.json reloads the details from disk, and rebuilds like a disk build" ["Backend", "Env", "Frontend", "Leaf", "Middle", "Types", "Lamdera.Live", "Lamdera.Repl"]

          edit "src/Leaf.elm" (T.replace "leaf-v2b" "leaf-v3")
          _ <- step "and keeps building in memory after that" ["Leaf", "Lamdera.Live"]

          scope "flushing writes elm-stuff that a disk build finds up to date" $ do
            io $ Lamdera.CLI.Live.flushMemory memDir store
            io $ MemoryCache.unregister store
            mem <- io $ compileLive memDir
            disk <- io $ compileLive diskDir
            case (mem, disk) of
              (Right (memJs, memFresh), Right (diskJs, _)) -> do
                expectEqual (Set.fromList ["Lamdera.Live"]) memFresh
                expect (memJs == diskJs)
              _ ->
                crash "a build failed after the flush"

          scope "a new store starts from that elm-stuff" $ do
            store2 <- io $ registerStore MemoryCache.InMemory memDir
            io $ writeIORef current store2
            edit "src/Leaf.elm" (T.replace "leaf-v3" "leaf-v4")
            _ <- step "after a restart" ["Leaf", "Lamdera.Live"]
            counters <- io $ MemoryCache.takeCounters store2
            -- Leaf's old interface, and every object but the fresh ones, came from disk
            expect (MemoryCache._diskReads counters > 0)
            _ <- step "and the next rebuild reads nothing from disk" ["Lamdera.Live"]
            counters2 <- io $ MemoryCache.takeCounters store2
            expectEqual 0 (MemoryCache._diskReads counters2)
            io $ MemoryCache.unregister store2
  in
  using setup cleanup test



-- BUILD


registerStore :: MemoryCache.Backing -> FilePath -> IO MemoryCache.Store
registerStore backing dir = do
  stuff <- R.getStuff (R.fromFilePath dir)
  MemoryCache.register backing stuff


stuffDir :: FilePath -> IO FilePath
stuffDir dir =
  R.stuffToFilePath <$> R.getStuff (R.fromFilePath dir)


{-| What Develop.compile does for `lamdera live`, returning the JS and the modules
compiled (Build.Fresh) instead of the HTML page.
-}
compileLive :: FilePath -> IO (Either String (BS.ByteString, Set.Set String))
compileLive dir = do
  harness <- Lamdera.CLI.Live.prepareLocalDev dir
  freshRef <- newIORef Set.empty
  result <-
    Ext.Common.withProjectRoot dir $
    R.withRootLock (R.fromFilePath dir) $ \writer stuff ->
      Task.run $
        do  let root = R.fromFilePath dir
            details <- Task.eio Exit.ReactorBadDetails $ Details.load writer Reporting.silent root stuff
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths writer Reporting.silent root stuff details (NE.List harness [])
            Task.io $ writeIORef freshRef $ Set.fromList [ Module.toChars name | Build.Fresh name _ _ <- Build._modules artifacts ]
            Lamdera.PostCompile.check details artifacts Exit.ReactorBadBuild
            memory <- Task.io $ MemoryCache.forStuff stuff
            fresh <- Task.io $ maybe (pure Nothing) Details.fromMemory memory
            case fresh of
              Just builtDetails -> Lamdera.TypeHash.buildCheckHashesFrom builtDetails artifacts
              Nothing -> Lamdera.TypeHash.buildCheckHashes artifacts
            Task.mapError Exit.ReactorBadGenerate $ Generate.dev stuff details artifacts
  fresh <- readIORef freshRef
  pure $
    case result of
      Right javascript -> Right (BSL.toStrict (B.toLazyByteString javascript), fresh)
      Left exit -> Left (Exit.toString (Exit.reactorToReport exit))


{-| The type hashes of a memory build, taken from its artifacts as Develop does with
--memory, and from a separate build of src/Types.elm as the default does.
-}
typeHashes :: FilePath -> IO (Either String ([T.Text], [T.Text]))
typeHashes dir = do
  harness <- Lamdera.CLI.Live.prepareLocalDev dir
  result <-
    Ext.Common.withProjectRoot dir $
    R.withRootLock (R.fromFilePath dir) $ \writer stuff ->
      Task.run $
        do  let root = R.fromFilePath dir
            details <- Task.eio Exit.ReactorBadDetails $ Details.load writer Reporting.silent root stuff
            artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths writer Reporting.silent root stuff details (NE.List harness [])
            Task.io $ do
              memory <- MemoryCache.forStuff stuff
              fresh <- maybe (pure Nothing) Details.fromMemory memory
              case fresh of
                Nothing ->
                  pure (Left "no Details in memory")

                Just builtDetails -> do
                  fromBuild <- Lamdera.TypeHash.calculateLamderaHashesFrom builtDetails artifacts
                  separate <- Lamdera.TypeHash.calculateLamderaHashes
                  pure $
                    case (fromBuild, separate) of
                      (Right (a, _), Right (b, _)) -> Right (a, b)
                      _ -> Left "a type hash calculation failed"
  pure $
    case result of
      Right hashes -> hashes
      Left exit -> Left (Exit.toString (Exit.reactorToReport exit))


hook :: BS.ByteString
hook =
  BSL.toStrict (B.toLazyByteString JS.patchHook)


withoutHook :: BS.ByteString -> BS.ByteString
withoutHook js =
  let (before, after) = BS.breakSubstring hook js in
  if BS.null after then js else before <> BS.drop (BS.length hook) after


isLeft :: Either a b -> Bool
isLeft e =
  case e of
    Left _ -> True
    Right _ -> False



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


touchElmJson :: FilePath -> IO ()
touchElmJson path = do
  sleep 10
  editFile path id


writeProject :: FilePath -> IO ()
writeProject dir = do
  rmdir dir
  Dir.createDirectoryIfMissing True (dir </> "src")
  writeUtf8 (dir </> "elm.json") elmJson
  writeUtf8 (dir </> "src/Types.elm") typesModule
  writeUtf8 (dir </> "src/Frontend.elm") frontendModule
  writeUtf8 (dir </> "src/Backend.elm") backendModule
  writeUtf8 (dir </> "src/Env.elm") "module Env exposing (..)\n\n\ndummyConfigItem =\n    \"\"\n"
  writeUtf8 (dir </> "src/Leaf.elm") "module Leaf exposing (..)\n\n\nleafText : String\nleafText =\n    \"leaf-v1\"\n\n\nleafJoin : String -> String -> String\nleafJoin a b =\n    a ++ b\n"
  writeUtf8 (dir </> "src/Middle.elm") "module Middle exposing (middleText)\n\nimport Leaf\n\n\nmiddleText : String\nmiddleText =\n    Leaf.leafJoin \"middle:\" Leaf.leafText\n"


-- what `lamdera init` writes: the live harness needs elm/bytes as a direct dependency
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
            "elm/browser": "1.0.2",
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "elm/html": "1.0.1",
            "elm/url": "1.0.0",
            "lamdera/codecs": "1.0.0",
            "lamdera/core": "1.0.0"
        },
        "indirect": {
            "elm/file": "1.0.5",
            "elm/http": "2.0.0",
            "elm/json": "1.1.4",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.5"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
|]


typesModule :: T.Text
typesModule = [text|
module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , color : Color
    }


type alias BackendModel =
    { counter : Int
    , color : Color
    }


type Color
    = Red
    | Green


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | Increment Int


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | CounterChanged Int Color
|]


frontendModule :: T.Text
frontendModule = [text|
module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Lamdera
import Middle
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key, message = "", color = Red }, Lamdera.sendToBackend (Increment 1) )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        CounterChanged n color ->
            ( { model | message = String.fromInt n, color = color }, Cmd.none )


colorName : Color -> String
colorName color =
    case color of
        Red ->
            "red"

        _ ->
            "another colour"


view model =
    { title = Middle.middleText
    , body = [ Html.text (model.message ++ colorName model.color) ]
    }
|]


backendModule :: T.Text
backendModule = [text|
module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { counter = 0, color = Green }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        Increment n ->
            ( { model | counter = model.counter + n }, Lamdera.sendToFrontend clientId (CounterChanged (model.counter + n) model.color) )
|]
