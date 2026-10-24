{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Live where

{- `lamdera live` functionalty -}

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Word (Word32, Word64)

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO, writeTVar, TVar)
import Control.Exception (finally, try, SomeException)
import qualified Language.Haskell.TH as TH
import Data.FileEmbed (bsToExp)
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap

import Snap.Core hiding (path, headers)
import qualified Data.CaseInsensitive as CI

import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String
import qualified Data.Utf8 as Utf8

import Lamdera
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import BroadcastChan
import Control.Timeout (timeout)
import qualified Network.WebSockets.Snap as WS
import SocketServer

import System.Entropy (getEntropy)
import Snap.Util.FileServe (
    getSafePath, serveDirectoryWith, defaultDirectoryConfig, defaultMimeTypes, mimeTypes, DirectoryConfig
  )
import Control.Monad (guard, mfilter)

import qualified Lamdera.CLI.Check
import qualified Lamdera.Relative
import qualified Lamdera.Version
import qualified Ext.Common
import qualified GHC.IO.Exception

import Control.Exception (onException)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Dynamic (fromDynamic)
import qualified GHC.Clock
import qualified GHC.Stats
import qualified System.Mem
import qualified Build
import qualified Elm.Details as Details
import qualified Ext.MemoryCache as MemoryCache
import qualified Generate.JavaScript as JS
import qualified Lamdera.HotPatch as HotPatch
import qualified File
import qualified Reporting.Task as Task
import qualified Root as R
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)



type LiveState = (TVar [Client], TVar (Maybe ClientId), BroadcastChan In Text, TVar Text)


init :: IO LiveState
init = do
  mClients <- liftIO $ SocketServer.clientsInit
  mLeader <- liftIO $ SocketServer.leaderInit
  mChan <- liftIO $ newBroadcastChan

  beState <- do
    bePath <- liftIO $ lamderaBackendDevSnapshotPath
    beText <- liftIO $ readUtf8Text bePath
    liftIO $ newTVarIO $
      case beText of
        Just text -> text
        Nothing -> "{\"t\":\"x\"}"

  pure (mClients, mLeader, mChan, beState)


withEnd :: LiveState -> IO () -> IO ()
withEnd (mClients, mLeader, mChan, beState) io = do
  let
    end = do
      debug "[backendSt] 🧠"
      text <- readTVarIO beState
      bePath <- lamderaBackendDevSnapshotPath
      writeUtf8 bePath text

  finally io end



-- Additional handler to serve files in /public from root / so that
-- image/asset references from Elm work locally same as in production
serveLamderaPublicFiles :: FilePath -> (FilePath -> Snap ()) -> Snap ()
serveLamderaPublicFiles root serveElm =
  do  file <- getSafePath
      let pubFile = root </> "public" </> file
      guard =<< liftIO (Dir.doesFileExist pubFile)
      -- debug $ "serving lamdera public files: " <> file
      serveElm pubFile <|> serveDirectoryWith directoryConfig (root </> "public")


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  defaultDirectoryConfig {
    mimeTypes =
      defaultMimeTypes
        & HashMap.insert ".md" "text/plain"
        & HashMap.insert ".webp" "image/webp"
  }


-- So that Elm's Navigation routing can work on any URL, serve any unmatched
-- non-extensioned paths to the "index" (in this case the Lamdera/Live.elm
-- harness as we're local in the reactor). Extensioned paths will continue to
-- the next handler, namely `error404` (see `run` fn at top of file)
serveUnmatchedUrlsToIndex :: FilePath -> (FilePath -> Snap()) -> Snap ()
serveUnmatchedUrlsToIndex root serveElm =
  do  file <- getSafePath
      guard (FP.takeExtension file == "")
      serveElm (lamderaCache root </> "Lamdera" </> "Live.elm")


prepareLocalDev :: FilePath -> IO FilePath
prepareLocalDev root = do
  overrideM <- Lamdera.Relative.readDir TE.decodeUtf8 "extra/LocalDev/runtime-src"

  -- This needs to be moved to an on-demand action, as it has to query production and
  -- thus isn't appropriate to run on every single recompile
  -- nextVersionInfo <- Lamdera.CLI.Check.getNextVersionInfo root
  -- Lamdera.CLI.Check.writeLamderaGenerated root True nextVersionInfo

  rpcExists <- doesFileExist $ root </> "src" </> "RPC.elm"

  let
    cache = lamderaCache root
    harnessPath = "Lamdera" </> "Live.elm"

    patchedContent path content =
      if path == harnessPath
        then content & replaceVersionMarker & replaceRpcMarker rpcExists
        else content

    processFile (path, content) =
      writeIfDifferent (cache </> path) $ patchedContent path content

    files = fromMaybe lamderaLocalDevDir overrideM

  mapM_ processFile files

  pure $ cache </> harnessPath


{-| The files prepareLocalDev writes, for the --memory source table (the file watcher
ignores elm-stuff).
-}
localDevFiles :: FilePath -> IO [FilePath]
localDevFiles root = do
  overrideM <- Lamdera.Relative.readDir TE.decodeUtf8 "extra/LocalDev/runtime-src"
  pure $ fmap ((lamderaCache root </>) . fst) (fromMaybe lamderaLocalDevDir overrideM)


replaceVersionMarker :: Text -> Text
replaceVersionMarker lamderaLive = do
  let (m,mi,p) = Lamdera.Version.raw
  lamderaLive & T.replace
    "( 0, 0, 0 )"
    (T.concat ["( ", show_ m , ", ", show_ mi , ", ", show_ p , " )"])


replaceRpcMarker :: Bool -> Text -> Text
replaceRpcMarker shouldReplace lamderaLive =
  if not shouldReplace
    then lamderaLive
    else
      lamderaLive
        & T.replace
          "-- MKRRI"
          "import RPC\n\
          \import LamderaRPC"
        & T.replace
          "-- MKRRC"
          "let\n\
          \                model =\n\
          \                    { userModel = m.bem }\n\
          \\n\
          \                ( newModel, newBeCmds ) =\n\
          \                    LamderaRPC.process\n\
          \                        (\\k v ->\n\
          \                            let\n\
          \                                x =\n\
          \                                    log k v\n\
          \                            in\n\
          \                            Cmd.none\n\
          \                        )\n\
          \                        rpcOut\n\
          \                        rpcArgsJson\n\
          \                        RPC.lamdera_handleEndpoints\n\
          \                        model\n\
          \            in\n\
          \            ( { m | bem = newModel.userModel, bemDirty = True }, Cmd.map BEMsg newBeCmds )\n\
          \            {-}"


lamderaLocalDevDir :: [(FilePath, Text)]
lamderaLocalDevDir =
  $(do
      bsPairs <- TH.runIO (Lamdera.Relative.readDir id "extra/LocalDev/runtime-src")
      let toTuple (fp, bs) = [| (fp, TE.decodeUtf8 $(bsToExp bs)) |]
      TH.ListE <$> mapM toTuple (fromMaybe [] bsPairs)
   )


refreshClients (mClients, mLeader, mChan, beState) =
  SocketServer.broadcastImpl mClients "{\"t\":\"r\"}" -- r is refresh, see live.js


serveWebsocket root (mClients, mLeader, mChan, beState) =
  do  file <- getSafePath
      guard (file == "_w")
      mKey <- getHeader "sec-websocket-key" <$> getRequest
      mSid <- getCookie "sid"

      randBytes <- liftIO $ getEntropy 20
      let newSid = BSL.toStrict $ B.toLazyByteString $ B.byteStringHex randBytes

      sessionId <-
        case mSid of
          Nothing -> do
            let cookie = Cookie "sid" newSid Nothing Nothing Nothing False False
            modifyResponse $ addResponseCookie cookie

            pure $ TE.decodeUtf8 $ newSid

          Just sid_ ->
            pure $ TE.decodeUtf8 $ cookieValue sid_

      case mKey of
        Just key -> do
          let onJoined clientId totalClients = do
                leaderChanged <- atomically $ do
                  leader <- readTVar mLeader
                  case leader of
                    Just leaderId ->
                      -- No change
                      pure False

                    Nothing -> do
                      -- If there's no leader, become the leader
                      writeTVar mLeader (Just clientId)
                      pure True

                onlyWhen leaderChanged $ do
                  sendToLeader mClients mLeader (\leader -> do
                      -- Tell the new leader about the backend state they need
                      readTVarIO beState
                    )
                  -- Tell everyone about the new leader (also causes actual leader to go active as leader)
                  broadcastLeader mClients mLeader

                SocketServer.broadcastImpl mClients $ "{\"t\":\"c\",\"s\":\"" <> sessionId <> "\",\"c\":\"" <> clientId <> "\"}"

                leader <- readTVarIO mLeader
                case leader of
                  Just leaderId ->
                    pure $ Just $ "{\"t\":\"s\",\"c\":\"" <> clientId <> "\",\"l\":\"" <> leaderId <> "\"}"

                  Nothing ->
                    -- Impossible
                    pure Nothing

              onReceive clientId text = do
                -- debugT $ "[socketRecieve ] " <> text
                if T.isPrefixOf "{\"t\":\"env\"," text
                  then do
                    -- This is a bit dodge, but avoids needing to pull in all of Aeson
                    setEnvMode root $ (T.splitOn "\"" text) !! 7

                    -- Touch the src/Env.elm file to make sure it gets recompiled
                    touch $ root </> "src" </> "Env.elm"

                    -- Mode has changed, force a refresh
                    -- Actually not needed, because the touch will do this for us!
                    -- SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                  else if T.isSuffixOf "\"t\":\"p\"}" text
                    then do
                      -- debug "[backendSt] 💾"
                      atomically $ writeTVar beState text
                      onlyWhen (textContains "force" text) $ do
                        debug "[refresh  ] 🔄 "
                        -- Force due to backend reset, force a refresh
                        SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                    else if T.isPrefixOf "{\"t\":\"ToBackend\"," text

                      then do
                        sendToLeader mClients mLeader (\l -> pure text)


                    else if T.isPrefixOf "{\"t\":\"qr\"," text
                      then do
                        -- debugT $ "RPC:↖️ " <> text
                        -- Query response, send it to the chan for pickup by awaiting HTTP endpoint
                        liftIO $ writeBChan mChan text
                        pure ()

                    else
                      SocketServer.broadcastImpl mClients text

          WS.runWebSocketsSnap $
            SocketServer.socketHandler mClients mLeader beState onJoined onReceive (TE.decodeUtf8 key) sessionId

        Nothing ->
          error404 "missing sec-websocket-key header"

openEditorHandler :: FilePath -> Snap ()
openEditorHandler root = do
  fullpath <- getSafePath
  debug $ "_x/editor fullpath: " ++ fullpath
  case FP.splitDirectories fullpath of
    "_x" : "editor" : rest -> do
      maybeRow <- getQueryParam "row"
      maybeColumn <- getQueryParam "column"

      let parseNonNegative = mfilter (>= 0) . readMaybe . Data.ByteString.Char8.unpack

      case (maybeRow >>= parseNonNegative, maybeColumn >>= parseNonNegative) of
        (Just row, Just column) ->
          serveEditorOpen root (FP.joinPath rest) row column

        _ ->
          error400PlainText "Unexpected request, expecting format: /_x/editor/<filename>?row=<row>&column=<column>"

    _ ->
      pass


serveBem :: LiveState -> Snap ()
serveBem (_ ,_ ,_ , beState) = do
  path <- getSafePath
  guard (path == "_x/bem")
  bemText <- liftIO $ readTVarIO beState
  writeText bemText


serveExperimental :: FilePath -> Snap ()
serveExperimental root = do
  fullpath <- T.pack <$> getSafePath
  let
    handlers =
      -- *nix dir paths
      [ ("_x/read", serveExperimentalRead root)
      , ("_x/write", serveExperimentalWrite root)
      , ("_x/list", serveExperimentalList root)
      -- Windows dir paths
      , ("_x\\read", serveExperimentalRead root)
      , ("_x\\write", serveExperimentalWrite root)
      , ("_x\\list", serveExperimentalList root)
      ]
  handlers
    & List.find (\(prefix, handler) ->
      prefix `T.isPrefixOf` fullpath
    )
    & fmap (\(prefix, handler) -> do
      let path =
            fullpath & T.replace (prefix <>  "/") ""  -- Strip when sub-dirs
                     & T.replace (prefix <>  "\\") "" -- Strip when sub-dirs windows
                     & T.replace prefix ""            -- Strip when root dir
      failIfNotExperimentalMode (handler path)
    )
    & withDefault pass


serveExperimentalRead :: FilePath -> Text -> Snap ()
serveExperimentalRead root path = do
  debug $ "_x/read received: " ++ show path
  let
    fullpath :: FilePath
    fullpath = root </> (T.unpack path)
  debug $ "_x/read: " ++ show fullpath
  exists_ <- liftIO $ Dir.doesFileExist fullpath
  if exists_
    then do
      sendFile fullpath
    else do
      error404 "file not found"


serveExperimentalWrite :: FilePath -> Text -> Snap ()
serveExperimentalWrite root path = do

  rbody <- readRequestBody _10MB
  debug $ "_x/write received: " ++ show path
  let
    fullpath :: FilePath
    fullpath = root </> (T.unpack path)
  debug $ "_x/write: " ++ show fullpath

  contentType :: Maybe BS.ByteString <- getHeader "Content-Type" <$> getRequest

  debug $ "_x/write: " ++ show contentType

  liftIO $ case contentType of
    Just "application/octet-stream" -> Lamdera.writeBinary fullpath rbody
    Just "image/jpeg"               -> Lamdera.writeBinary fullpath rbody
    Just "image/png"                -> Lamdera.writeBinary fullpath rbody
    Just "image/gif"                -> Lamdera.writeBinary fullpath rbody
    Just "image/webp"               -> Lamdera.writeBinary fullpath rbody
    Just "image/svg+xml"            -> Lamdera.writeBinary fullpath rbody
    Just "application/pdf"          -> Lamdera.writeBinary fullpath rbody

    _ ->
      writeIfDifferent fullpath (TL.toStrict $ TLE.decodeUtf8 rbody)

  jsonResponse $ B.byteString $ "{ written: '" <> TE.encodeUtf8 (T.pack fullpath) <> "'}"


serveExperimentalList :: FilePath -> Text -> Snap ()
serveExperimentalList root path = do
  debug $ "_x/list received: " ++ show path
  let
    fullpath :: FilePath
    fullpath = root </> (T.unpack path)
  debug $ "_x/list: " ++ show fullpath
  exists_ <- liftIO $ Dir.doesDirectoryExist fullpath
  if exists_
    then do
      files <- liftIO $ Dir.getDirectoryContents fullpath
      files
        & E.list (E.jsonString . Utf8.fromChars)
        & E.encode
        & jsonResponse

    else do
      error404 "folder not found"


serveEditorOpen :: FilePath -> FilePath -> Int -> Int -> Snap ()
serveEditorOpen root path row column = do
  debug $ "_x/editor received: " ++ show path
  let fullpath = root </> path
  debug $ "_x/editor: " ++ show fullpath
  exists_ <- liftIO $ Dir.doesFileExist fullpath
  if exists_
    then do
      tryOpenInDetectedEditor root fullpath row column

    else do
      error400PlainText "File not found"


tryOpenInDetectedEditor :: FilePath -> FilePath -> Int -> Int -> Snap ()
tryOpenInDetectedEditor root file row column = do
  res <- liftIO $ sequence (editors root)
  case justs res of
    [] ->
      error404 "No supported editors found. See the Lamdera docs for more information."

    (editor, openEditor):_ -> do
      debug "📝  found the following editors, opening first:"
      justs res & fmap fst & show & debug

      runRes <- liftIO (try (openEditor file row column) :: IO (Either SomeException (GHC.IO.Exception.ExitCode, String, String)))
      case runRes of
        Right (exit, stdout, stderr) ->
          case exit of
            GHC.IO.Exception.ExitSuccess ->
              noContentResponse

            GHC.IO.Exception.ExitFailure exitCode ->
              error400PlainText $ Ext.Common.stringToBuilder $ "exit " <> show exitCode <> ": " <> stdout <> stderr

        Left err ->
          error400PlainText $ Ext.Common.stringToBuilder $ show err


type EditorOpenIO = (FilePath -> Int -> Int -> IO (GHC.IO.Exception.ExitCode, String, String))


editors :: FilePath -> [IO (Maybe (B.Builder, EditorOpenIO))]
editors projectRoot =
  [ detectEditor "custom-*nix"
      (Dir.doesFileExist (projectRoot </> "openEditor.sh"))
      (\file row column -> Ext.Common.c_ (projectRoot </> "openEditor.sh") [file, show row, show column] "")

  , detectEditor "custom-windows"
      (do
        exists <- Dir.doesFileExist (projectRoot </> "openEditor.bat")
        pure $ exists && ostype == Windows
      )
      (\file row column -> Ext.Common.c_ (projectRoot </> "openEditor.bat") [file, show row, show column] "")

  , detectExecutable "code-insiders"
      (\executablePath file row column -> do
        Ext.Common.c_ executablePath [ "-g", file <> ":" <> show row <> ":" <> show column] ""
      )

  , detectExecutable "code"
      (\executablePath file row column -> do
        Ext.Common.c_ executablePath [ "-g", file <> ":" <> show row <> ":" <> show column] ""
      )

  , detectEditor "intellij-ce"
      (Dir.doesDirectoryExist "/Applications/IntelliJ IDEA CE.app")
      (\file row column -> do
        -- IntelliJ seems to number it's columns from 1 index
        Ext.Common.c_ "open" ["-na", "IntelliJ IDEA CE.app", "--args", "--line", show row, "--column", show (column - 1), file] ""
      )

  , detectEditor "intellij"
      (Dir.doesDirectoryExist "/Applications/IntelliJ IDEA.app")
      (\file row column -> do
        -- IntelliJ seems to number it's columns from 1 index
        Ext.Common.c_ "open" ["-na", "IntelliJ IDEA.app", "--args", "--line", show row, "--column", show (column - 1), file] ""
      )
  ]


detectExecutable :: B.Builder -> (FilePath -> EditorOpenIO) -> IO (Maybe (B.Builder, EditorOpenIO))
detectExecutable executableName fn = do
  pathM <- Dir.findExecutable $ Ext.Common.builderToString executableName
  pure $ case pathM of
    Just path -> Just (executableName, fn path)
    _ -> Nothing


detectEditor :: B.Builder -> IO Bool -> EditorOpenIO -> IO (Maybe (B.Builder, EditorOpenIO))
detectEditor editorName editorExistsCheck openIO = do
  exists <- editorExistsCheck
  if exists
    then
      pure $ Just (editorName, openIO)
    else
      pure Nothing


-- | Generate RPC request payload based on content type and request data
generateRpcRequestPayload ::
  Maybe BS.ByteString  -- ^ Content-Type header
  -> BSL.ByteString    -- ^ Request body
  -> BS.ByteString     -- ^ Endpoint
  -> Text              -- ^ Session ID
  -> Text              -- ^ Request ID
  -> E.Value           -- ^ Request headers JSON
  -> Text              -- ^ Final JSON payload
generateRpcRequestPayload contentType rbody endpoint sid reqId requestHeadersJson =
  let
    -- Unfortunately the JSON string encoding logic is hidden inside Data.Aeson.Encoding.Internal
    -- so off we go with all the silly format hops
    escapeJsonString :: Text -> Text
    escapeJsonString t = A.text t & A.encodingToLazyByteString & BSL.toStrict & TE.decodeUtf8

    escapedBody =
      rbody & TLE.decodeUtf8 & TL.toStrict & escapeText

    escapeText :: Text -> E.Value
    escapeText t =
      t & escapeJsonString & T.unpack & Utf8.fromChars
        -- E.string quotes the string, but because we had to route through Aeson for escaping, it's already quoted
        -- so instead we have to construct the raw E.Value
        & (E.String . Json.String.toBuilder)

    fallbackStringBody = rpcPayload ("st", escapedBody)

    rpcPayload value =
      E.object
          [ ("t", E.chars "q")
          , ("s", E.text sid)
          , ("e", E.text $ TE.decodeUtf8 endpoint)
          , ("r", E.text reqId)
          , ("h", E.String $ Ext.Common.textToBuilder $ encodeToText requestHeadersJson)
          , value
          ]
        & encodeToText

    encodeToText encoder = encoder & E.encode & B.toLazyByteString & BSL.toStrict & TE.decodeUtf8

    requestPayload =
      case contentType of
        Just "application/octet-stream" ->
          let
            body = T.pack $ show $ BSL.unpack rbody
          in
          rpcPayload ("i", E.text body)

        Just "application/json" ->
          rpcPayload ("j", escapedBody)

        Just "application/x-www-form-urlencoded" ->
          let
            -- @TODO it would be nice to rework this to use E.* in future
            body =
              Snap.Core.parseUrlEncoded (BSL.toStrict rbody)
                & Map.toList
                & fmap (\(key, vals) ->
                  let
                    values =
                      case vals of
                        [] -> "null"
                        val:[] -> TE.decodeUtf8 val & escapeJsonString
                        _ ->
                          vals
                            & fmap (escapeJsonString . TE.decodeUtf8)
                            & T.intercalate ","
                            & (\v -> T.concat ["[", v, "]"])
                  in
                  T.concat ["\"", TE.decodeUtf8 key, "\":", values]
                )
                & (\v -> T.concat ["{", (v & T.intercalate ","), "}"])
          in
          rpcPayload ("j", escapeText body)

        Just other ->
          fallbackStringBody
        Nothing ->
          fallbackStringBody
  in
  requestPayload


serveRpc (mClients, mLeader, mChan, beState) port = do

  mEndpoint <- getParam "endpoint"
  rbody <- readRequestBody _10MB
  mSid <- getCookie "sid"
  requestHeaders :: [(BS.ByteString, BS.ByteString)] <- fmap (\(cs, s) -> (CI.foldedCase cs, s)) <$> listHeaders <$> getRequest

  -- E.chars perfoms character escaping, as header values can often have " within them
  let requestHeadersJson = requestHeaders & nodeRequestHeaders & fmap (Ext.Common.bsToUtf8 *** (E.chars . Ext.Common.bsToString)) & E.object

  -- Just the media type, so `application/json; charset=utf-8` is JSON like it is for production's body parsers
  contentType :: Maybe BS.ByteString <-
    fmap (TE.encodeUtf8 . T.toLower . T.strip . T.takeWhile (/= ';') . TE.decodeUtf8) . getHeader "Content-Type" <$> getRequest

  debug $ "RPC:↘️ " ++ show (contentType, mEndpoint, mSid, rbody)

  randBytes <- liftIO $ getEntropy 20
  let newSid = BSL.toStrict $ B.toLazyByteString $ B.byteStringHex randBytes

  sid <-
    case mSid of
      Nothing -> do
        let cookie = Cookie "sid" newSid Nothing Nothing Nothing False False
        modifyResponse $ addResponseCookie cookie

        pure $ TE.decodeUtf8 $ newSid

      Just sid_ ->
        pure $ TE.decodeUtf8 $ cookieValue sid_

  onlyWhen (mEndpoint == Nothing) $ error500 "no endpoint present"

  -- Using UUIDv4 here instead of UUIDv1 like in production is merely a matter
  -- of ergonomics; The UUIDv1 package only has `nextUUID :: IO (Maybe UUID)`
  -- as it returns Nothing for requests too close together, so using UUIDv4
  -- was more practical than implementing a UUIDv1 with retry
  reqId <- liftIO $ UUID.toText <$> UUID.nextRandom
  outChan <- newBChanListener mChan

  let
    endpoint =
      case mEndpoint of
        Just endpoint_ ->
          endpoint_

        Nothing ->
          -- Should be impossible given we already checked above
          error "impossible: no endpoint present"

    requestPayload = generateRpcRequestPayload contentType rbody endpoint sid reqId requestHeadersJson

    loopRead :: IO Text
    loopRead = do
      res <- readBChan outChan
      case res of
        Just chanText
          | textContains reqId chanText -> do
              debugT $ "loopRead decoding: " <> chanText
              pure chanText
          | otherwise -> loopRead
        Nothing -> loopRead

  leader <- liftIO $ readTVarIO mLeader
  case leader of
    Just leaderId -> do
      liftIO $ sendToLeader mClients mLeader (\leader_ -> pure requestPayload)

      let seconds = 10
      chanTextM <- liftIO $ timeout seconds $ loopRead

      case chanTextM of
        Just chanText -> do
          let
            -- Decoded with aeson rather than Json.Decode: that parser is written for elm.json and
            -- rejects negative and fractional numbers, which a response value can hold (lamdera/compiler#108).
            decoder :: Aeson.Value -> Aeson.Parser (Int, BS.ByteString, [(Text, Text)], RpcResponseBody)
            decoder =
              Aeson.withObject "rpc response" $ \o -> do
                statusCode <- o Aeson..:? "c" Aeson..!= 200
                statusText <- o Aeson..:? "ct" Aeson..!= "OK"
                headers <- o Aeson..:? "h" Aeson..!= AesonKeyMap.empty
                bytes <- o Aeson..:? "i"
                string <- o Aeson..:? "vs"
                -- live.js serialises the Json.Value with the browser's JSON.stringify, the same
                -- function the production runtime's res.json uses, so numbers come out identically
                json <- o Aeson..:? "vj"
                let
                  -- Same precedence as the production runtime (runtime/backend.js)
                  body =
                    case (bytes, string) of
                      (Just ints, _) -> RpcBytes (BS.pack (fmap fromIntegral (ints :: [Int])))
                      (_, Just s) -> RpcString s
                      _ -> RpcJson json
                pure
                  ( statusCode
                  , Ext.Common.textToBs statusText
                  , headers & AesonKeyMap.toList & fmap (\(k, v) -> (AesonKey.toText k, v))
                  , body
                  )

            decodeResult = Aeson.eitherDecodeStrict (TE.encodeUtf8 chanText) >>= Aeson.parseEither decoder
          case decodeResult of
            Right (statusCode, statusText, headers, body) -> do
              let
                isHeader name (key, _) = T.toLower key == name

                -- Production blocks Set-Cookie from RPC responses, so cookies can't be stuffed across *.lamdera.app
                (blockedCookies, appHeaders) = headers & List.partition (isHeader "set-cookie")

                appContentType = appHeaders & List.find (isHeader "content-type") & fmap (expressSetContentType . snd)

                -- The Content-Type express ends up sending: res.end for bytes leaves it as is,
                -- res.send and res.json default it and force the charset to utf-8
                (contentType, bodyBuilder, bodyLog) =
                  case body of
                    RpcBytes bs ->
                      (appContentType & fromMaybe "application/octet-stream", B.byteString bs, "[bytes]")
                    RpcString s ->
                      (appContentType & fromMaybe "text/html" & expressSetCharsetUtf8, TE.encodeUtf8Builder s, s)
                    RpcJson j ->
                      (appContentType & fromMaybe "application/json" & expressSetCharsetUtf8, j & maybe mempty TE.encodeUtf8Builder, j & fromMaybe "")

              debugT $ "RPC:↙️  response:" <> bodyLog
              debug $ show (statusCode, statusText)
              debug $ show headers

              blockedCookies & mapM_ (\(key, value) ->
                  liftIO $ atomicPutStrLn $ T.unpack $ "Blocked Set-Cookie header from RPC response: " <> key <> ": " <> value
                )

              appHeaders & filter (not . isHeader "content-type") & mapM_ (\(key, value) ->
                  modifyResponse $ setHeader (Ext.Common.textToBs key & CI.mk) (Ext.Common.textToBs value)
                )
              modifyResponse $ setContentType (Ext.Common.textToBs contentType)

              modifyResponse $ setResponseStatus statusCode statusText
              writeBuilder bodyBuilder

            Left jsonProblem -> do
              debugT $ "😢 rpc response decoding failed: " <> T.pack jsonProblem <> "\n" <> chanText
              writeBuilder $ B.byteString $ "rpc response decoding failed for " <> TE.encodeUtf8 chanText


        Nothing -> do
          debugT $ "⏰ RPC timed out for:" <> requestPayload
          -- What production answers when the app doesn't respond within 10s
          modifyResponse $ setResponseStatus 503 "Service Unavailable"
          modifyResponse $ setContentType "application/json; charset=utf-8"
          writeBuilder "{\"error\":{}}"


    Nothing -> do
      debug "RPC: no active leader"
      error503 $ B.string8 $ "it appears no browser instances are running! Please open http://localhost:" <> show port <> " in a browser."


-- Request headers the way node's req.headers has them, which is what the app gets in production:
-- names lowercased, and a repeated header joined, except the ones node keeps only the first of
nodeRequestHeaders :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
nodeRequestHeaders headers =
  let
    firstOnly =
      [ "age", "authorization", "content-length", "content-type", "etag", "expires", "from", "host"
      , "if-modified-since", "if-unmodified-since", "last-modified", "location", "max-forwards"
      , "proxy-authorization", "referer", "retry-after", "server", "user-agent"
      ]

    merge acc (key, value) =
      case lookup key acc of
        Nothing -> acc ++ [(key, value)]
        Just existing
          | key `elem` firstOnly -> acc
          | otherwise ->
              let joined = existing <> (if key == "cookie" then "; " else ", ") <> value
              in acc & fmap (\(k, v) -> if k == key then (k, joined) else (k, v))
  in
  -- (Snap lists a repeated header's values newest first)
  List.foldl' merge [] (reverse headers)


data RpcResponseBody
  = RpcBytes BS.ByteString
  | RpcString Text
  | RpcJson (Maybe Text) -- Nothing when the Json.Value was undefined, which res.json sends as an empty body


-- What express's res.set does to a Content-Type an app sets: a text or JSON type with no charset gets utf-8
expressSetContentType :: Text -> Text
expressSetContentType value =
  let
    hasCharset =
      value & T.splitOn ";" & drop 1
        & any (\param -> param & T.stripStart & T.stripPrefix "charset" & maybe False (T.isPrefixOf "=" . T.stripStart))

    mediaType = value & T.takeWhile (/= ';')

    isUtf8Type =
      "text/" `T.isPrefixOf` mediaType
        || "application/javascript" `T.isPrefixOf` mediaType
        || "application/json" `T.isPrefixOf` mediaType
  in
  if not hasCharset && isUtf8Type then value <> "; charset=utf-8" else value


-- What express's res.send and res.json do to the Content-Type of a string body: re-format it with charset=utf-8
expressSetCharsetUtf8 :: Text -> Text
expressSetCharsetUtf8 value =
  case T.splitOn ";" value of
    [] -> value
    mediaType : params ->
      let
        parsedParams =
          params
            & fmap (\param -> param & T.breakOn "=" & (\(k, v) -> (k & T.strip & T.toLower, v & T.drop 1 & T.strip)))
            & filter (\(k, _) -> k /= "" && k /= "charset")
      in
      ((mediaType & T.strip & T.toLower) : (("charset", "utf-8") : parsedParams & List.sortOn fst & fmap (\(k, v) -> k <> "=" <> v)))
        & T.intercalate "; "


-- andThen :: (a -> D.Decoder e b) -> D.Decoder e a -> D.Decoder e b
-- andThen callback (D.Decoder runA) =
--   D.Decoder $ \value ->
--     do  a <- runA value
--         let (D.Decoder runB) = callback a
--         runB value

_10MB :: Word64
_10MB =
  10000000 -- 10MB limit

logger :: BS.ByteString -> IO ()
logger =
  (\bs ->
    atomicPutStrLn $ T.unpack $ TE.decodeUtf8 bs
  )


noContentResponse :: Snap ()
noContentResponse = do
  modifyResponse $ setResponseStatus 204 "No Content"
  r <- getResponse
  finishWith r


jsonResponse :: B.Builder -> Snap ()
jsonResponse s =
  do  modifyResponse $ setContentType "application/json; charset=utf-8"
      writeBuilder s
      r <- getResponse
      finishWith r

httpError :: Int -> BS.ByteString -> B.Builder -> Snap ()
httpError statusCode errorTitle s =
  do  modifyResponse $ setResponseStatus statusCode errorTitle
      modifyResponse $ setContentType "application/json; charset=utf-8"
      writeBuilder $ "{\"error\":\"" <> s <> "\"}"
      r <- getResponse
      finishWith r


error404 :: B.Builder -> Snap ()
error404 s =
  -- writeBuilder $ Generate.makePageHtml "NotFound" Nothing
  httpError 404 "Not Found" s

error500 :: B.Builder -> Snap ()
error500 s =
  do  modifyResponse $ setResponseStatus 500 "Internal server error"
      modifyResponse $ setContentType "application/json; charset=utf-8"
      writeBuilder $ "{\"error\":\"" <> s <> "\"}"
      r <- getResponse
      finishWith r

error503 :: B.Builder -> Snap ()
error503 s =
  do  modifyResponse $ setResponseStatus 503 "Service Unavailable"
      modifyResponse $ setContentType "application/json; charset=utf-8"
      writeBuilder $ "{\"error\":\"" <> s <> "\"}"
      r <- getResponse
      finishWith r

error400PlainText :: B.Builder -> Snap ()
error400PlainText s =
  do  modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setContentType "text/plain; charset=utf-8"
      writeBuilder s
      r <- getResponse
      finishWith r


failIfNotExperimentalMode :: Snap () -> Snap ()
failIfNotExperimentalMode handler =
  if isExperimental_
    then handler
    else error503 "Only available with EXPERIMENTAL=1"


passOnIndex pwd =
  if (pwd == ".")
    then do
      debug "passing on / index"
      pass
    else
      pure ()


-- x = 1



-- THE LIVE SESSION: what `lamdera live` keeps between rebuilds (see Ext.MemoryCache)


{-| Keep this project's build artifacts as decoded values for the rest of the process, see
Ext.MemoryCache, either only in memory (--memory: after the first build no rebuild reads
or writes the project's elm-stuff until flushMemory) or on disk as always, with the
values kept as well. Either way the rebuilds reuse the JS of unchanged definitions and
the Wire3 codecs, skip the type hashes when nothing they hash changed, and pages in
freeze mode get hot patches.

LAMDERA_LIVE_SESSION=0 keeps nothing (each rebuild as a separate `lamdera make` would,
the way `lamdera live` worked before), unless --memory is given.
-}
startSession :: FilePath -> Bool -> IO (Maybe MemoryCache.Store)
startSession root inMemory = do
  off <- (== Just "0") <$> lookupEnv "LAMDERA_LIVE_SESSION"
  if off && not inMemory
    then pure Nothing
    else do
      stuff <- R.getStuff (R.fromFilePath root)
      store <- MemoryCache.register (if inMemory then MemoryCache.InMemory else MemoryCache.OnDisk) stuff
      onlyWhen inMemory $
        atomicPutStrLn "🧠 --memory: build artifacts are kept in memory, elm-stuff is written on exit."
      pure (Just store)


isInMemory :: Maybe MemoryCache.Store -> Bool
isInMemory maybeStore =
  fmap MemoryCache.backing maybeStore == Just MemoryCache.InMemory


{-| A build that throws (rather than reporting compile errors) may have stopped half way
through updating the store, so drop it: the next build starts from elm-stuff again,
which memory mode has left as the consistent state of the last disk build.
-}
withMemoryReset :: Maybe MemoryCache.Store -> IO a -> IO a
withMemoryReset maybeStore io =
  case maybeStore of
    Nothing -> io
    Just store -> io `onException` MemoryCache.reset store


{-| On the way out, write what was compiled in memory to elm-stuff, so the next
`lamdera live` or `lamdera make` starts warm.
-}
withMemoryFlush :: FilePath -> Maybe MemoryCache.Store -> IO () -> IO ()
withMemoryFlush root maybeStore io =
  case maybeStore of
    Just store | MemoryCache.backing store == MemoryCache.InMemory -> io `finally` flushMemory root store
    _ -> io -- OnDisk elm-stuff is always up to date


flushMemory :: FilePath -> MemoryCache.Store -> IO ()
flushMemory root store = do
  start <- GHC.Clock.getMonotonicTimeNSec
  R.withRootLock (R.fromFilePath root) $ \writer stuff -> do
    let writeDetails dyn =
          case fromDynamic dyn of
            Just details -> File.writeBytes writer (R.details stuff) Details.eDetails (details :: Details.Details)
            Nothing -> pure ()
    (elmos, elmis) <- MemoryCache.flush store writer stuff writeDetails
    end <- GHC.Clock.getMonotonicTimeNSec
    onlyWhen (elmos + elmis > 0) $
      atomicPutStrLn $ "🧠 --memory: wrote " <> show elmos <> " .elmo and " <> show elmis <> " .elmi files to elm-stuff in " <> show ((end - start) `div` 1000000) <> "ms"



-- SOURCES (see Ext.MemoryCache)


memorySourcesChanged :: Maybe MemoryCache.Store -> [FilePath] -> IO ()
memorySourcesChanged maybeStore events =
  case maybeStore of
    Nothing -> pure ()
    Just store -> do
      _ <- MemoryCache.sourcesChanged store events
      pure ()


{-| Every few seconds, look at the kept sources on disk again, and rebuild if any changed
without the watcher saying so.
-}
startSourceReconcile :: Maybe MemoryCache.Store -> ([FilePath] -> IO ()) -> IO ()
startSourceReconcile maybeStore recompile =
  case maybeStore of
    Just store | MemoryCache.backing store == MemoryCache.InMemory ->
      Ext.Common.trackedForkIO "Lamdera.CLI.Live.startSourceReconcile" $ forever $ do
        threadDelay 5000000
        changed <- MemoryCache.reconcileSources store
        onlyWhen (not (null changed)) $ do
          atomicPutStrLn $ "🧠 --memory: changed without a watcher event: " <> show changed
          recompile changed
    _ -> pure () -- OnDisk the sources are read from disk on every build


reconcileSources :: Maybe MemoryCache.Store -> ([FilePath] -> IO ()) -> IO ()
reconcileSources maybeStore onChanged =
  case maybeStore of
    Nothing -> pure ()
    Just store -> do
      changed <- MemoryCache.reconcileSources store
      onlyWhen (not (null changed)) $ onChanged changed



-- HOT PATCHES (see Lamdera.HotPatch)


{-| In a live session, tells the page which build it runs (this goes after the Elm JS, so a
full hot reload of a new page sets it on the scope it evaluates that page into).
-}
jsVersionMarker :: MemoryCache.Store -> IO B.Builder
jsVersionMarker store = do
  version <- MemoryCache.jsVersion store
  pure $ "\n;this.__lamderaJsVersion = " <> B.intDec version <> ";\n"


{-| What a page running build `from` gets from /_patch: "patch <latest>\n" and the JS to
run, or "full <why not>".
-}
hotPatch :: Maybe MemoryCache.Store -> Int -> IO BS.ByteString
hotPatch maybeStore from = do
  start <- GHC.Clock.getMonotonicTimeNSec
  result <-
    case maybeStore of
      Nothing ->
        pure $ Left "no live session"

      Just store -> do
        since <- MemoryCache.jsBuildsSince store from
        pure $
          case since of
            Nothing ->
              Left "that build is no longer kept"

            Just (latest, dyns) ->
              case traverse fromDynamic dyns of
                Just listings@(_ : newer@(_ : _)) ->
                  let steps = zipWith HotPatch.diff listings newer in
                  case [ reason | HotPatch.Full reason <- steps ] of
                    reason : _ ->
                      Left reason

                    [] ->
                      let statements = concat [ patch | HotPatch.Patch patch <- steps ] in
                      Right (latest, statements, HotPatch.render statements (JS._lsExports (last listings)))

                _ ->
                  Left "no newer build"
  case result of
    Left reason -> do
      timingLog $ "🔥 hot patch from build " <> show from <> ": full reload, " <> reason
      pure $ "full " <> Data.ByteString.Char8.pack reason

    Right (latest, statements, js) -> do
      let bytes = BSL.toStrict $ B.toLazyByteString $ "patch " <> B.intDec latest <> "\n" <> js
      end <- GHC.Clock.getMonotonicTimeNSec
      timingLog $ "🔥 hot patch from build " <> show from <> " to " <> show latest <> ": "
        <> show (length statements) <> " statements, " <> show (BS.length bytes) <> " bytes, in "
        <> show ((end - start) `div` 1000000) <> "ms"
      pure bytes


timingLog :: String -> IO ()
timingLog message = do
  enabled <- isTimingEnabled
  onlyWhen enabled $ atomicPutStrLn message



-- TIMING
--
-- With LAMDERA_LIVE_TIMING set, each rebuild prints how long it took to have fresh JS,
-- how many modules were compiled, what the memory store did, and the heap after a major
-- GC (needs +RTS -T). For measuring --memory against the default (and LAMDERA_LIVE_SESSION=0).


timedRebuild :: Maybe MemoryCache.Store -> IO a -> IO a
timedRebuild maybeStore io = do
  enabled <- isTimingEnabled
  if not enabled
    then io
    else do
      gcBefore <- gcCounters
      start <- GHC.Clock.getMonotonicTimeNSec
      !result <- io
      end <- GHC.Clock.getMonotonicTimeNSec
      gcAfter <- gcCounters
      counters <-
        case maybeStore of
          Just store -> do
            c <- MemoryCache.takeCounters store
            (ifaces, objects) <- MemoryCache.size store
            pure $ " store: " <> show (MemoryCache._ifaceHits c) <> " interface hits, " <> show (MemoryCache._objectHits c) <> " object hits, " <> show (MemoryCache._diskReads c) <> " disk reads, " <> show (MemoryCache._sourceFs c) <> " source stats/reads, " <> show (MemoryCache._parseHits c) <> " parses kept, " <> show (MemoryCache._staleReads c) <> " changed by someone else, holding " <> show ifaces <> " interfaces and " <> show objects <> " objects (" <> show (MemoryCache.backing store) <> ")."
          Nothing -> pure ""
      timings <- atomicModifyIORef' phases (\ps -> ([], reverse ps))
      let total = (end - start) `div` 1000000
          inPhases = sum (map snd timings) `div` 1000000
          breakdown = concatMap (\(label, ns) -> label <> " " <> show (ns `div` 1000000) <> "ms, ") timings
      let gc =
            case (gcBefore, gcAfter) of
              (Just (ns0, n0, gcs0), Just (ns1, n1, gcs1)) ->
                " GC " <> show ((ns1 - ns0) `div` 1000000) <> "ms in " <> show (gcs1 - gcs0) <> " collections, " <> show (n1 - n0) <> " major."
              _ -> ""
      atomicPutStrLn $ "🕐 live rebuild: " <> show total <> "ms (" <> breakdown <> "rest " <> show (total - inPhases) <> "ms)." <> gc <> counters
      heap <- heapSummary
      onlyWhen (heap /= "") $ atomicPutStrLn $ "🕐 heap after GC:" <> heap
      pure result


{-# NOINLINE phases #-}
phases :: IORef [(String, Word64)]
phases =
  unsafePerformIO $ newIORef []


{-| Time a step of the live build, for the LAMDERA_LIVE_TIMING breakdown. -}
phase :: String -> Task.Task x a -> Task.Task x a
phase label task = do
  enabled <- Task.io isTimingEnabled
  if not enabled
    then task
    else do
      t0 <- Task.io GHC.Clock.getMonotonicTimeNSec
      a <- task
      t1 <- Task.io GHC.Clock.getMonotonicTimeNSec
      Task.io $ atomicModifyIORef' phases (\ps -> ((label, t1 - t0) : ps, ()))
      pure a


reportFresh :: Build.Artifacts -> IO ()
reportFresh artifacts = do
  enabled <- isTimingEnabled
  onlyWhen enabled $ do
    let fresh = [ () | Build.Fresh _ _ _ <- Build._modules artifacts ]
    atomicPutStrLn $ "🕐 live build: " <> show (length fresh) <> " modules compiled, " <> show (length (Build._modules artifacts)) <> " in the build"


isTimingEnabled :: IO Bool
isTimingEnabled = do
  v <- lookupEnv "LAMDERA_LIVE_TIMING"
  pure (v /= Nothing)


gcCounters :: IO (Maybe (GHC.Stats.RtsTime, Word32, Word32))
gcCounters = do
  enabled <- GHC.Stats.getRTSStatsEnabled
  if not enabled
    then pure Nothing
    else do
      stats <- GHC.Stats.getRTSStats
      pure $ Just (GHC.Stats.gc_elapsed_ns stats, GHC.Stats.major_gcs stats, GHC.Stats.gcs stats)


heapSummary :: IO String
heapSummary = do
  enabled <- GHC.Stats.getRTSStatsEnabled
  if not enabled
    then pure ""
    else do
      System.Mem.performMajorGC
      stats <- GHC.Stats.getRTSStats
      let mb n = show (n `div` (1024 * 1024)) <> "MB"
      pure $ " " <> mb (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc stats))
        <> " live, " <> mb (GHC.Stats.gcdetails_mem_in_use_bytes (GHC.Stats.gc stats)) <> " in use, peak "
        <> mb (GHC.Stats.max_mem_in_use_bytes stats) <> " in use."
