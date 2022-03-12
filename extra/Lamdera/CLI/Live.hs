{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Live where

{- `lamdera live` functionalty
-}

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Word (Word64)

import qualified System.Directory as Dir
import System.FilePath as FP
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import Control.Exception (finally, throw)
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)


import Snap.Core


import qualified Develop.Generate.Help as Generate
import qualified Develop.StaticFiles as StaticFiles
import qualified Json.Decode as D
import qualified Json.Encode as E
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
import Snap.Util.FileServe
import Control.Monad (guard, void)

import qualified Lamdera.CLI.Check


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
-- serveLamderaPublicFiles :: Snap ()
serveLamderaPublicFiles serveElm serveFilePretty =
  do  file <- getSafePath
      let pubFile = "public" </> file
      guard =<< liftIO (Dir.doesFileExist pubFile)
      -- debug $ "serving lamdera public files: " <> file
      serveElm pubFile <|> serveFilePretty pubFile


-- So that Elm's Navigation routing can work on any URL, serve any unmatched
-- non-extensioned paths to the "index" (in this case the src/LocalDev.elm
-- harness as we're local in the reactor). Extensioned paths will continue to
-- the next handler, namely `error404` (see `run` fn at top of file)
-- serveUnmatchedUrlsToIndex :: Snap ()
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

  isDebug <- isDebug
  overrideM <- readUtf8Text overridePath

  -- This needs to be moved to an on-demand action, as it has to query production and
  -- thus isn't appropriate to run on every single recompile
  -- nextVersionInfo <- Lamdera.CLI.Check.getNextVersionInfo root
  -- Lamdera.CLI.Check.writeLamderaGenerated root True nextVersionInfo

  if isDebug
    then do
      rpcExists <- doesFileExist $ root </> "src" </> "RPC.elm"

      case overrideM of
        Just override -> do
          writeIfDifferent harnessPath (override & replaceRpcMarkers rpcExists)

        Nothing ->
          writeIfDifferent harnessPath (lamderaLocalDev & replaceRpcMarkers rpcExists)

    else do
      writeIfDifferent harnessPath lamderaLocalDev

  pure harnessPath


replaceRpcMarkers :: Bool -> Text -> Text
replaceRpcMarkers shouldReplace localdev =
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
  -- @TODO fix this back later... conflicts with the change directory command in ghci live reload
  T.decodeUtf8 $(bsToExp =<< runIO (BS.readFile ("extra/LocalDev/LocalDev.elm")))

  -- $(bsToExp =<< runIO (BS.readFile ("/Users/mario/dev/projects/lamdera-compiler/extra/LocalDev/LocalDev.elm")))



refreshClients (mClients, mLeader, mChan, beState) =
  SocketServer.broadcastImpl mClients "{\"t\":\"r\"}" -- r is refresh, see live.js


serveWebsocket (mClients, mLeader, mChan, beState) =
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

                SocketServer.broadcastImpl mClients $ "{\"t\":\"c\",\"s\":\"" <> sessionId <> "\",\"c\":\""<> clientId <> "\"}"

                leader <- atomically $ readTVar mLeader
                case leader of
                  Just leaderId ->
                    pure $ Just $ "{\"t\":\"s\",\"c\":\"" <> clientId <> "\",\"l\":\"" <> leaderId <> "\"}"

                  Nothing ->
                    -- Impossible
                    pure Nothing

              onReceive clientId text = do
                if T.isPrefixOf "{\"t\":\"env\"," text
                  then do
                    root <- liftIO $ getProjectRoot
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

                        debugT $ "🍕  rpc response:" <> text
                        -- Query response, send it to the chan for pickup by awaiting HTTP endpoint
                        liftIO $ writeBChan mChan text
                        pure ()

                    else
                      SocketServer.broadcastImpl mClients text

          WS.runWebSocketsSnap $ SocketServer.socketHandler mClients mLeader beState onJoined onReceive (T.decodeUtf8 key) sessionId

        Nothing ->
          error404 "missing sec-websocket-key header"

serveExperimental :: FilePath -> Snap ()
serveExperimental root = do
  fullpath <- T.pack <$> getSafePath
  let
    handlers =
      [ ("_x/read", serveExperimentalRead root)
      , ("_x/write", serveExperimentalWrite root)
      , ("_x/list", serveExperimentalList root)
      ]
  handlers
    & List.find (\(prefix, handler) ->
      prefix `T.isPrefixOf` fullpath
    )
    & fmap (\(prefix, handler) -> do
      let path =
            fullpath & T.replace (prefix <>  "/") "" -- Strip when sub-dirs
                     & T.replace prefix ""           -- Strip when root dir
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

  liftIO $ writeIfDifferent fullpath (TL.toStrict $ TL.decodeUtf8 rbody)
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


serveRpc (mClients, mLeader, mChan, beState) port = do

  mEndpoint <- getParam "endpoint"
  rbody <- readRequestBody _10MB
  mSid <- getCookie "sid"
  contentType <- getHeader "Content-Type" <$> getRequest

  debug $ "RPC received: " ++ show (contentType, mEndpoint, mSid, rbody)

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
          error "no endpoint present"

    -- Splice some extra info onto the RPC JSON request
    payload =
      case contentType of
        Just "application/octet-stream" ->
          let
            body = T.pack $ show $ BSL.unpack rbody
          in
          -- t s e r i j
          "{\"t\":\"q\",\"s\":\""<> sid <>
          "\",\"e\":\"" <> T.decodeUtf8 endpoint <>
          "\",\"r\":\""<> reqId <>
          "\",\"i\":" <> body <>
          ",\"j\":null}"

        Just "application/json" ->
          let
            body = TL.toStrict $ TL.decodeUtf8 rbody
          in
          "{\"t\":\"q\",\"s\":\""<> sid <>
          "\",\"e\":\"" <> T.decodeUtf8 endpoint <>
          "\",\"r\":\""<> reqId <>
          "\",\"i\":[],\"j\":" <> body <> "}"

        Just "application/x-www-form-urlencoded" ->
          let
            escapeJsonString = T.replace "\"" "\\\""

            body = T.pack $ show $ BSL.unpack rbody

            body2 =
              Snap.Core.parseUrlEncoded (BSL.toStrict rbody)
                & Map.toList
                & fmap (\(key, vals) ->
                  let
                    values =
                      case vals of
                        [] -> "null"
                        val:[] -> "\"" <> (T.decodeUtf8 val & escapeJsonString) <> "\""
                        _ ->
                          vals
                            & fmap (\v -> "\"" <> (T.decodeUtf8 v & escapeJsonString) <> "\"")
                            & T.intercalate ","
                            & (\v -> "[" <> v <> "]")
                  in
                  "\"" <> T.decodeUtf8 key <> "\":" <> values
                )
                & (\v ->
                  "{" <> (v & T.intercalate ",") <> "}"
                    -- & T.encodeUtf8
                    -- & Snap.Core.urlEncode
                    -- & T.decodeUtf8
                )
                -- & (\v ->
                --   "\"" <> v <> "\""
                -- )

          in
          -- t s e r i j
          "{\"t\":\"q\",\"s\":\""<> sid <>
          "\",\"e\":\"" <> T.decodeUtf8 endpoint <>
          "\",\"r\":\""<> reqId <>
          "\",\"i\":[],\"j\":" <> body2 <> "}"

        Nothing -> do
          error "invalid Content-Type"

    loopRead :: IO (Either (D.Error x) B.Builder, Text)
    loopRead = do
      res <- readBChan outChan

      case res of
        Nothing ->
          loopRead

        Just chanText ->
          if textContains reqId chanText
            then do
              let
                decoder :: D.Decoder err E.Value
                decoder =
                  D.oneOf
                    [ D.field "i" D.value
                    , D.field "v" D.value
                    ]

              pure (D.fromByteString decoder (T.encodeUtf8 chanText) & fmap E.encode, chanText)

            else
              loopRead


  leader <- liftIO $ atomically $ readTVar mLeader
  case leader of
    Just leaderId -> do
      liftIO $ sendToLeader mClients mLeader (\leader -> pure payload)

      result <- liftIO $ timeout 2 $ loopRead

      case result of
        Just (result, chanText) ->
          case result of
            Right value ->
              if textContains "\"error\":" chanText then
                error400 (B.byteString $ T.encodeUtf8 chanText)
              else
                writeBuilder value

            Left jsonProblem -> do
              debugT $ "😢 rpc response decoding failed for " <> chanText
              writeBuilder $ B.byteString $ "rpc response decoding failed for " <> T.encodeUtf8 chanText


        Nothing -> do
          debugT $ "⏰ RPC timed out for:" <> payload
          writeBuilder "error:timeout"


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

error404 :: B.Builder -> Snap ()
error404 s =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "application/json; charset=utf-8"
      -- writeBuilder $ Generate.makePageHtml "NotFound" Nothing
      writeBuilder $ "{\"error\":\"" <> s <> "\"}"
      r <- getResponse
      finishWith r

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
