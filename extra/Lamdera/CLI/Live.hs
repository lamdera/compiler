{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Live where

{- `lamdera live` functionalty
-}

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
import Control.Exception (finally, throw)
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)


import Snap.Core


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
import Network.Info
import System.Entropy
import Snap.Util.FileServe
import Control.Monad (guard, void)


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
serveUnmatchedUrlsToIndex serveElm =
  do  file <- getSafePath
      guard (takeExtension file == "")

      root <- liftIO $ getProjectRoot

      let
        cache = lamderaCache root
        harnessPath = cache </> "LocalDev.elm"

      serveElm harnessPath
      -- Cleanup causes issues because we might have a slew of tabs
      -- We can restore this when we further optimise `serveElm` to have a debounce cache!
      -- liftIO $ Dir.removeFile harnessPath
      -- liftIO $ callCommand $ "rm " <> harnessPath <> " || true " -- less exception-ey on double-reload!


prepareLocalDev = do
  root <- getProjectRoot

  let
    cache = lamderaCache root
    harnessPath = cache </> "LocalDev.elm"

  isDebug <- isDebug

  onlyWhen isDebug $ do
    let overridePath = "/Users/mario/dev/projects/lamdera-compiler/extra/LocalDev/LocalDev.elm"
    overrideExists <- doesFileExist overridePath
    onlyWhen overrideExists $ do
      debug $ "🚧 OVERRIDE from extra/LocalDev/LocalDev.elm"
      copyFile overridePath harnessPath

    rpcExists <- doesFileExist $ root </> "src" </> "RPC.elm"

    onlyWhen rpcExists $ do

      -- Inject missing imports
        replaceInFile
          "-- MKRRI"
          "import RPC\n\
          \import LamderaRPC"
          harnessPath

      -- Replace body implementation
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

  pure harnessPath



lamderaLocalDev :: BS.ByteString
lamderaLocalDev =
  -- @TODO fix this back later... conflicts with the change directory command in ghci live reload
  $(bsToExp =<< runIO (BS.readFile ("extra/LocalDev/LocalDev.elm")))
  -- $(bsToExp =<< runIO (BS.readFile ("/Users/mario/dev/projects/lamdera-compiler/extra/LocalDev/LocalDev.elm")))


normalLocalDevWrite = do

  root <- getProjectRoot

  let
    cache = lamderaCache root
    harnessPath = cache </> "LocalDev.elm"
  mkdir cache

  harnessExists <- doesFileExist harnessPath
  if harnessExists
    then do
      now <- getCurrentTime
      modified <- Dir.getModificationTime harnessPath
      accessed <- Dir.getAccessTime harnessPath

      -- debug $ "🚧 n:" <> show now
      -- debug $ "🚧 a:" <> show accessed
      -- debug $ "🚧 m:" <> show modified

      if diffUTCTime now modified > 60
        then do
          -- File was last modified more than 5 seconds ago, okay to rewrite
          -- debug $ "🚧 writing, more than 5:" <> show (diffUTCTime now modified)
          BS.writeFile harnessPath lamderaLocalDev
        else do
          -- Modified recently, don't rewrite to prevent compiler issues
          -- when multiple tabs are open for lamdera live
          -- debug $ "🚧 skipping write! "  <> show (diffUTCTime now modified)
          pure ()
    else do
      -- No file exists yet, must be a new project
      debug "🚧 🆕"
      BS.writeFile harnessPath lamderaLocalDev


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
                if Text.isPrefixOf "{\"t\":\"envMode\"," text
                  then do
                    root <- liftIO $ getProjectRoot
                    -- This is a bit dodge, but avoids needing to pull in all of Aeson
                    setEnvMode root $ (Text.splitOn "\"" text) !! 7

                    -- Touch the src/Env.elm file to make sure it gets recompiled
                    touch $ root </> "src" </> "Env.elm"

                    -- Mode has changed, force a refresh
                    -- Actually not needed, because the touch will do this for us!
                    -- SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                  else if Text.isSuffixOf "\"t\":\"p\"}" text
                    then do
                      -- debug "[backendSt] 💾"
                      atomically $ writeTVar beState text
                      onlyWhen (textContains "force" text) $ do
                        debug "[refresh  ] 🔄 "
                        -- Force due to backend reset, force a refresh
                        SocketServer.broadcastImpl mClients "{\"t\":\"r\"}"

                    else if Text.isPrefixOf "{\"t\":\"ToBackend\"," text

                      then do
                        sendToLeader mClients mLeader (\l -> pure text)


                    else if Text.isPrefixOf "{\"t\":\"qr\"," text

                      then do

                        debugT $ "🍕  rpc response:" <> text
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
      error503 "it appears no browser instances are running! Please open http://localhost:8000 in a browser."


-- andThen :: (a -> D.Decoder e b) -> D.Decoder e a -> D.Decoder e b
-- andThen callback (D.Decoder runA) =
--   D.Decoder $ \value ->
--     do  a <- runA value
--         let (D.Decoder runB) = callback a
--         runB value


logger =
  (\bs ->
    atomicPutStrLn $ Text.unpack $ T.decodeUtf8 bs
  )


error500 :: B.Builder -> Snap ()
error500 s =
  do  modifyResponse $ setResponseStatus 500 "Internal server error"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "error:" <> s

error503 :: B.Builder -> Snap ()
error503 s =
  do  modifyResponse $ setResponseStatus 503 "Service Unavailable"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "error: " <> s


error400 :: B.Builder -> Snap ()
error400 s =
  do  modifyResponse $ setResponseStatus 400 "Bad Request"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "error: " <> s


passOnIndex pwd =
  if (pwd == ".")
    then do
      debug "passing on / index"
      pass
    else
      pure ()


network = do
  ns <- getNetworkInterfaces

  ns
    & fmap ipv4
    & filter (\v -> show v /= "0.0.0.0")
    & filter (\v -> show v /= "127.0.0.1")
    & mapM_ (putStr . show)


showInterface n = name n ++ "\n"
               ++ "  IPv4: " ++ show (ipv4 n) ++ "\n"
               ++ "  IPv6: " ++ show (ipv6 n) ++ "\n"
               ++ "  MAC:  " ++ show (mac n) ++ "\n"
