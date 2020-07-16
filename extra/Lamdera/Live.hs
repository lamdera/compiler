{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Live
  ( Lamdera.Live.init
  , withEnd
  , serveWebsocket
  , serveRpc
  , serveLamderaPublicFiles
  , serveUnmatchedUrlsToIndex)
  where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import qualified System.Directory as Dir
import System.FilePath as FP
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import Control.Exception (finally)

import Snap.Core

import Elm.Project.Json
import Elm.Project.Summary
import qualified Develop.Generate.Help as Generate
import qualified Develop.StaticFiles as StaticFiles
import qualified Json.Decode as D
import qualified Json.Encode as E

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

-- Bytes
import System.Entropy

-- import Snap.Http.Server
import Snap.Util.FileServe
import Control.Monad (guard, void)
import Lamdera.Filewatch


type LiveState = (TVar [Client], TVar (Maybe ClientId), BroadcastChan In Text, TVar Text)

-- (mClients, mLeader, mChan, beState)
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
      Lamdera.debug "[backendSt] 🧠"
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
      -- Lamdera.debug $ "serving lamdera public files: " <> file
      serveElm pubFile <|> serveFilePretty pubFile


-- So that Elm's Navigation routing can work on any URL, serve any unmatched
-- non-extensioned paths to the "index" (in this case the src/LocalDev.elm
-- harness as we're local in the reactor). Extensioned paths will continue to
-- the next handler, namely `error404` (see `run` fn at top of file)
-- serveUnmatchedUrlsToIndex :: Snap ()
serveUnmatchedUrlsToIndex serveElm =
  do  file <- getSafePath
      guard (takeExtension file == "")

      root <- liftIO $ getProjectRoot
      let harnessPath = root </> "lamdera-stuff/alpha/LocalDev.elm"
      liftIO $ Lamdera.mkdir $ root </> "lamdera-stuff/alpha"

      isDebug <- liftIO $ Lamdera.isDebug

      let
        normalLocalDevWrite = liftIO $ do
          harnessExists <- Lamdera.doesFileExist harnessPath
          if harnessExists
            then do
              now <- getCurrentTime
              modified <- Dir.getModificationTime harnessPath
              accessed <- Dir.getAccessTime harnessPath

              debug $ "🚧 n:" <> show now
              debug $ "🚧 a:" <> show accessed
              debug $ "🚧 m:" <> show modified

              if diffUTCTime now modified > 60
                then do
                  -- File was last modified more than 5 seconds ago, okay to rewrite
                  debug $ "🚧 writing, more than 5:" <> show (diffUTCTime now modified)
                  BS.writeFile harnessPath StaticFiles.lamderaLocalDev
                else do
                  -- Modified recently, don't rewrite to prevent compiler issues
                  -- when multiple tabs are open for lamdera live
                  debug $ "🚧 skipping write! "  <> show (diffUTCTime now modified)
                  pure ()
            else do
              -- No file exists yet, must be a new project
              debug "🚧 🆕"
              BS.writeFile harnessPath StaticFiles.lamderaLocalDev

      liftIO $ do
        if isDebug
          then do
            let overridePath = "/Users/mario/dev/projects/elmx/ui/browser/src/LocalDev.elm"
            overrideExists <- Lamdera.doesFileExist overridePath
            if overrideExists
              then do
                debug $ "🚧 OVERRIDE from elmx/ui/browser/src/LocalDev.elm"
                Lamdera.copyFile overridePath harnessPath
              else
                normalLocalDevWrite

            rpcExists <- Lamdera.doesFileExist $ root </> "src" </> "RPC.elm"

            onlyWhen rpcExists $ do

              -- Inject missing imports
              liftIO $
                replaceInFile
                  "-- MKRRI"
                  "import RPC\n\
                  \import LamderaRPC"
                  harnessPath
              -- Replace body implementation
              liftIO $
                replaceInFile
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
                  harnessPath

          else
            normalLocalDevWrite

      serveElm harnessPath
      -- Cleanup causes issues because we might have a slew of tabs
      -- We can restore this when we further optimise `serveElm` to have a debounce cache!
      -- liftIO $ Dir.removeFile harnessPath
      -- liftIO $ callCommand $ "rm " <> harnessPath <> " || true " -- less exception-ey on double-reload!


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
                if Text.isPrefixOf "{\"t\":\"envMode\"," text
                  then do
                    root <- liftIO $ getProjectRoot
                    -- This is a bit dodge, but avoids needing to pull in all of Aeson
                    Lamdera.setEnvMode root $ (Text.splitOn "\"" text) !! 7

                    -- Touch the src/Env.elm file to make sure it gets recompiled
                    Lamdera.touch $ root </> "src" </> "Env.elm"

                    -- Mode has changed, force a refresh
                    -- Actually not needed, because the touch will do this for us!
                    -- SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                  else if Text.isPrefixOf "{\"t\":\"BackendModel\"," text
                    then do
                      Lamdera.debug "[backendSt] 💾"
                      atomically $ writeTVar beState text
                      onlyWhen (textContains "force" text) $ do
                        Lamdera.debug "[refresh  ] 🔄 "
                        -- Force due to backend reset, force a refresh
                        SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                    else if Text.isPrefixOf "{\"t\":\"ToBackend\"," text

                      then do
                        sendToLeader mClients mLeader (\l -> pure text)


                    else if Text.isPrefixOf "{\"t\":\"qr\"," text

                      then do

                        Lamdera.debugT $ "🍕  rpc response:" <> text
                        -- Query response, send it to the chan for pickup by awaiting HTTP endpoint
                        liftIO $ writeBChan mChan text
                        pure ()

                    else
                      SocketServer.broadcastImpl mClients text

          WS.runWebSocketsSnap $ SocketServer.socketHandler mClients mLeader beState onJoined onReceive (T.decodeUtf8 key) sessionId


        Nothing ->
          error404


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ Generate.makePageHtml "NotFound" Nothing


serveRpc (mClients, mLeader, mChan, beState) = do

  mEndpoint <- getParam "endpoint"
  rbody <- readRequestBody 10000000 -- 10MB limit
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
            body = Text.pack $ show $ BSL.unpack rbody
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

        Nothing -> do
          error "invalid Content-Type"

    loopRead = do
      res <- readBChan outChan

      case res of
        Nothing ->
          loopRead

        Just chanText ->
          if textContains reqId chanText
            then do
              let
                decoder =
                  D.oneOf
                    [ D.at ["i"] (D.list D.int)
                        & D.andThen (\intList ->
                          let
                            intList_ :: [Word8]
                            intList_ = fmap fromIntegral intList
                          in
                          D.succeed (B.byteString $ BS.pack $ intList_)
                        )

                    , D.at ["v"] D.value
                        & D.andThen (\json ->
                          D.succeed (E.encode $ D.toEncodeValue json)
                        )
                    ]

              case D.parse "rpc-resp" id decoder (T.encodeUtf8 chanText) of
                Right value ->
                  pure $ writeBuilder value

                Left jsonProblem -> do
                  Lamdera.debugT $ "😢 rpc response decoding failed for " <> chanText
                  pure $ writeBuilder $ B.byteString $ "rpc response decoding failed for " <> T.encodeUtf8 chanText

            else
              loopRead


  leader <- liftIO $ atomically $ readTVar mLeader
  case leader of
    Just leaderId -> do
      liftIO $ sendToLeader mClients mLeader (\leader -> pure payload)

      result <- liftIO $ timeout 2 $ loopRead

      case result of
        Just builder -> do
          builder
        Nothing -> do
          Lamdera.debugT $ "⏰ RPC timed out for:" <> payload
          writeBuilder "error:timeout"


    Nothing -> do
      debug "RPC: no active leader"
      error503 "it appears no browser instances are running! Please open http://localhost:8000 in a browser."



error500 :: B.Builder -> Snap ()
error500 s =
  do  modifyResponse $ setResponseStatus 500 "Internal server error"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "error:" <> s
