{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Live where

{- `lamdera live` functionalty -}

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import GHC.Word (Word64)

import qualified System.Directory as Dir
import System.FilePath as FP
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import Control.Exception (finally, throw)
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)
import qualified Data.Aeson.Encoding as A

import Snap.Core hiding (path, headers)
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.Bifunctor (first)

import qualified Develop.Generate.Help as Generate
import qualified Develop.StaticFiles as StaticFiles
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String
import qualified Data.Utf8 as Utf8

import Lamdera
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import BroadcastChan
import Control.Timeout
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import SocketServer
import Data.Word (Word8)
import System.Process

import System.Entropy
import Snap.Util.FileServe (
    getSafePath, serveDirectoryWith, defaultDirectoryConfig, defaultMimeTypes, mimeTypes, MimeMap, DirectoryConfig
  )
import Control.Monad (guard, void)

import qualified Lamdera.CLI.Check
import qualified Lamdera.Relative
import qualified Lamdera.Version
import qualified Ext.Common



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
      text <- atomically $ readTVar beState
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
  }


-- So that Elm's Navigation routing can work on any URL, serve any unmatched
-- non-extensioned paths to the "index" (in this case the src/LocalDev.elm
-- harness as we're local in the reactor). Extensioned paths will continue to
-- the next handler, namely `error404` (see `run` fn at top of file)
serveUnmatchedUrlsToIndex :: FilePath -> (FilePath -> Snap()) -> Snap ()
serveUnmatchedUrlsToIndex root serveElm =
  do  file <- getSafePath
      guard (takeExtension file == "")
      serveElm (lamderaCache root </> "LocalDev.elm")


prepareLocalDev :: FilePath -> IO FilePath
prepareLocalDev root = do
  let
    cache = lamderaCache root
    harnessPath = cache </> "LocalDev.elm"
    overridePath = "/Users/mario/dev/projects/lamdera-compiler/extra/LocalDev/LocalDev.elm"

  overrideM <- readUtf8Text overridePath

  -- This needs to be moved to an on-demand action, as it has to query production and
  -- thus isn't appropriate to run on every single recompile
  -- nextVersionInfo <- Lamdera.CLI.Check.getNextVersionInfo root
  -- Lamdera.CLI.Check.writeLamderaGenerated root True nextVersionInfo

  rpcExists <- doesFileExist $ root </> "src" </> "RPC.elm"

  case overrideM of
    Just override -> do
      writeIfDifferent harnessPath
        (override
          & replaceVersionMarker
          & replaceRpcMarker rpcExists
        )

    Nothing ->
      writeIfDifferent harnessPath
        (lamderaLocalDev
          & replaceVersionMarker
          & replaceRpcMarker rpcExists
        )

  pure harnessPath


replaceVersionMarker :: Text -> Text
replaceVersionMarker localdev = do
  let (m,mi,p) = Lamdera.Version.raw
  localdev & T.replace
    "( 0, 0, 0 )"
    (T.concat ["( ", show_ m , ", ", show_ mi , ", ", show_ p , " )"])


replaceRpcMarker :: Bool -> Text -> Text
replaceRpcMarker shouldReplace localdev =
  if not shouldReplace
    then localdev
    else
      localdev
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


lamderaLocalDev :: Text
lamderaLocalDev =
  T.decodeUtf8 $(bsToExp =<< runIO (Lamdera.Relative.readByteString "extra/LocalDev/LocalDev.elm"))


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

            pure $ T.decodeUtf8 $ newSid

          Just sid_ ->
            pure $ T.decodeUtf8 $ cookieValue sid_

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
                      atomically $ readTVar beState
                    )
                  -- Tell everyone about the new leader (also causes actual leader to go active as leader)
                  broadcastLeader mClients mLeader

                SocketServer.broadcastImpl mClients $ "{\"t\":\"c\",\"s\":\"" <> sessionId <> "\",\"c\":\"" <> clientId <> "\"}"

                leader <- atomically $ readTVar mLeader
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
            SocketServer.socketHandler mClients mLeader beState onJoined onReceive (T.decodeUtf8 key) sessionId

        Nothing ->
          error404 "missing sec-websocket-key header"

openEditorHandler :: FilePath -> Snap ()
openEditorHandler root = do
  fullpath <- T.pack <$> getSafePath
  let
    handlers =
      -- *nix dir paths
      [ ("_x/editor", serveEditorOpen root)
      -- Windows dir paths
      , ("_x\\editor", serveEditorOpen root)
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
      handler path
    )
    & withDefault pass


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

    _ ->
      writeIfDifferent fullpath (TL.toStrict $ TL.decodeUtf8 rbody)

  jsonResponse $ B.byteString $ "{ written: '" <> T.encodeUtf8 (T.pack fullpath) <> "'}"


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
        & E.list (E.string . Utf8.fromChars)
        & E.encode
        & jsonResponse

    else do
      error404 "folder not found"


serveEditorOpen :: FilePath -> Text -> Snap ()
serveEditorOpen root path = do
  debug $ "_x/editor received: " ++ show path
  case path & T.splitOn ":" of
    file:row:column:xs -> do
      let fullpath = (root </> T.unpack file)
      debug $ "_x/editor: " ++ show fullpath
      exists_ <- liftIO $ Dir.doesFileExist fullpath
      if exists_
        then do
          tryOpenInDetectedEditor root fullpath row column

        else do
          error404 "file not found"
    _ ->
      error404 "unexpected identifier, expecting format: <filename>:<row>:<column>"


tryOpenInDetectedEditor :: FilePath -> FilePath -> Text -> Text -> Snap ()
tryOpenInDetectedEditor root file row column = do
  res <- liftIO $ mapM id (editors root)
  case justs res of
    [] ->
      -- @TODO give more helpful error that guides user how to configure things?
      error404 "No supported editors found"

    (editor, openEditor):xs -> do
      debug "📝  found the following editors, opening first:"
      justs res & fmap fst & show & debug

      liftIO $ openEditor file row column
      jsonResponse $ "{ status: 'tried opening editor " <> editor <> "' }"


type EditorOpenIO = (FilePath -> Text -> Text -> IO String)


editors :: FilePath -> [IO (Maybe (B.Builder, EditorOpenIO))]
editors projectRoot =
  [ detectEditor "custom-*nix"
      (Dir.doesFileExist (projectRoot </> "openEditor.sh"))
      (\file row column -> Ext.Common.cq_ (projectRoot </> "openEditor.sh") [file, T.unpack row, T.unpack column] "")

  , detectEditor "custom-windows"
      (do
        exists <- Dir.doesFileExist (projectRoot </> "openEditor.bat")
        pure $ exists && ostype == Windows
      )
      (\file row column -> Ext.Common.cq_ (projectRoot </> "openEditor.bat") [file, T.unpack row, T.unpack column] "")

  , detectExecutable "code-insiders"
      (\executablePath file row column -> do
        Ext.Common.c_ executablePath [ "-g", file <> ":" <> T.unpack row <> ":" <> T.unpack column] ""
      )

  , detectExecutable "code"
      (\executablePath file row column -> do
        Ext.Common.c_ executablePath [ "-g", file <> ":" <> T.unpack row <> ":" <> T.unpack column] ""
      )

  , detectEditor "intellij-ce"
      (Dir.doesDirectoryExist "/Applications/IntelliJ IDEA CE.app")
      (\file row column -> do
        let column_ :: Int = column & readMaybeText & withDefault 1
        -- IntelliJ seems to number it's columns from 1 index
        Ext.Common.cq_  "open" ["-na", "IntelliJ IDEA CE.app", "--args", "--line", T.unpack row, "--column", show (column_ - 1), file] ""
      )

  , detectEditor "intellij"
      (Dir.doesDirectoryExist "/Applications/IntelliJ IDEA.app")
      (\file row column -> do
        let column_ :: Int = column & readMaybeText & withDefault 1
        -- IntelliJ seems to number it's columns from 1 index
        Ext.Common.cq_  "open" ["-na", "IntelliJ IDEA.app", "--args", "--line", T.unpack row, "--column", show (column_ - 1), file] ""
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


serveRpc (mClients, mLeader, mChan, beState) port = do

  mEndpoint <- getParam "endpoint"
  rbody <- readRequestBody _10MB
  mSid <- getCookie "sid"
  headers :: [(BS.ByteString, BS.ByteString)] <- fmap (\(cs, s) -> (CI.original cs, s)) <$> listHeaders <$> getRequest

  -- E.chars perfoms character escaping, as header values can often have " within them
  let headersJson = headers & fmap (Ext.Common.bsToUtf8 *** (E.chars . Ext.Common.bsToString)) & E.object

  contentType :: Maybe BS.ByteString <- getHeader "Content-Type" <$> getRequest

  debug $ "RPC:↘️ " ++ show (contentType, mEndpoint, mSid, rbody)

  randBytes <- liftIO $ getEntropy 20
  let newSid = BSL.toStrict $ B.toLazyByteString $ B.byteStringHex randBytes

  sid <-
    case mSid of
      Nothing -> do
        let cookie = Cookie "sid" newSid Nothing Nothing Nothing False False
        modifyResponse $ addResponseCookie cookie

        pure $ T.decodeUtf8 $ newSid

      Just sid_ ->
        pure $ T.decodeUtf8 $ cookieValue sid_

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

    -- Unfortunately the JSON string encoding logic is hidden inside Data.Aeson.Encoding.Internal
    -- so off we go with all the silly format hops
    escapeJsonString :: Text -> Text
    escapeJsonString t = A.text t & A.encodingToLazyByteString & BSL.toStrict & T.decodeUtf8

    escapedBody =
      rbody & TL.decodeUtf8 & TL.toStrict & escapeText

    escapeText :: Text -> E.Value
    escapeText t =
      t & escapeJsonString & T.unpack & Utf8.fromChars
        -- E.string quotes the string, but because we had to route through Aeson for escaping, it's already quoted
        -- so instead we have to construct the raw E.Value
        & (E.String . Json.String.toBuilder)

    fallbackStringBody = rpcPayload ("st", escapedBody)

    rpcPayload value =
      E.object
          [ ("t", E.string "q")
          , ("s", E.text sid)
          , ("e", E.text $ T.decodeUtf8 endpoint)
          , ("r", E.text reqId)
          , ("h", E.String $ Ext.Common.textToBuilder $ encodeToText headersJson)
          , value
          ]
        & encodeToText

    encodeToText encoder = encoder & E.encode & B.toLazyByteString & BSL.toStrict & T.decodeUtf8

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
                        val:[] -> T.concat ["\"", (T.decodeUtf8 val & escapeJsonString), "\""]
                        _ ->
                          vals
                            & fmap (\v -> T.concat ["\"", (T.decodeUtf8 v & escapeJsonString), "\""])
                            & T.intercalate ","
                            & (\v -> T.concat ["[", v, "]"])
                  in
                  T.concat ["\"", T.decodeUtf8 key, "\":", values]
                )
                & (\v -> T.concat ["{", (v & T.intercalate ","), "}"])
          in
          rpcPayload ("j", escapeText body)

        Just other ->
          fallbackStringBody
        Nothing ->
          fallbackStringBody

    loopRead :: IO (Either (D.Error x) (Text, B.Builder), Text)
    loopRead = do
      res <- readBChan outChan

      case res of
        Nothing ->
          loopRead

        Just chanText ->
          if textContains reqId chanText
            then do
              let
                decoder :: D.Decoder err (Text, B.Builder)
                decoder =
                  D.oneOf
                    [ D.field "i" (D.value & fmap (\v -> ("i", E.encode v))) -- Bytes
                    , D.field "v" (D.value & fmap (\v -> ("v", E.encode v))) -- Json.Value
                    , D.field "vs" (D.string & fmap (\v -> ("vs", Json.String.toBuilder v))) -- String
                    ]

                result =
                  D.fromByteString decoder (T.encodeUtf8 chanText)

              -- debugT $ "loopRead decoding: " <> chanText
              pure (result, chanText)

            else
              loopRead


  leader <- liftIO $ atomically $ readTVar mLeader
  case leader of
    Just leaderId -> do
      liftIO $ sendToLeader mClients mLeader (\leader_ -> pure requestPayload)

      let seconds = 10
      resultPair <- liftIO $ timeout seconds $ loopRead

      case resultPair of
        Just (result, chanText) ->
          case result of
            Right (t, value) ->
              if textContains "\"error\":" chanText
                then do
                  debugT $ "RPC:↙️  error:" <> chanText
                  error400 (B.byteString $ T.encodeUtf8 chanText)
                else do
                  let response = TL.toStrict $ TL.decodeUtf8 $ B.toLazyByteString value
                  debugT $ "RPC:↙️  response:" <> response
                  onlyWhen (t == "i") (modifyResponse $ setContentType "application/octet-stream")
                  onlyWhen (t == "v") (modifyResponse $ setContentType "application/json; charset=utf-8")
                  onlyWhen (t == "vs") (modifyResponse $ setContentType "text/plain; charset=utf-8")
                  writeBuilder value

            Left jsonProblem -> do
              debugT $ "😢 rpc response decoding failed for " <> chanText
              writeBuilder $ B.byteString $ "rpc response decoding failed for " <> T.encodeUtf8 chanText


        Nothing -> do
          debugT $ "⏰ RPC timed out for:" <> requestPayload
          writeBuilder $ B.byteString $ T.encodeUtf8 $ "error:timeout:" <> show_ seconds <> "s"


    Nothing -> do
      debug "RPC: no active leader"
      error503 $ B.string8 $ "it appears no browser instances are running! Please open http://localhost:" <> show port <> " in a browser."


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
    atomicPutStrLn $ T.unpack $ T.decodeUtf8 bs
  )

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

error400 :: B.Builder -> Snap ()
error400 s =
  do  modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setContentType "application/json; charset=utf-8"
      writeBuilder $ "{\"error\":\"" <> s <> "\"}"
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


x = 1
