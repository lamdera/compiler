{-# LANGUAGE OverloadedStrings #-}
module Develop
  ( Flags(..)
  , run
  )
  where


import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import Control.Monad (guard, void)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception (finally)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import qualified System.Directory as Dir
import System.FilePath as FP
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Elm.Project as Project
import qualified Develop.Generate.Help as Generate
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Generate.Output as Output
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task


import Lamdera
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import SocketServer
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word (Word8)
import System.Process

import Elm.Project.Json
import Elm.Project.Summary

import qualified Json.Decode as D
import qualified Json.Encode as E

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import BroadcastChan
import Control.Timeout

import Lamdera.Filewatch

-- RUN THE DEV SERVER


data Flags =
  Flags
    { _port :: Maybe Int
    }


run :: () -> Flags -> IO ()
run () (Flags maybePort) =
  let
    port =
      maybe 8000 id maybePort
  in
    do
        liftIO $ Lamdera.stdoutSetup

        putStrLn $ "Go to <http://localhost:" ++ show port ++ "> to run your Lamdera project."

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

        Lamdera.Filewatch.watch mClients mLeader

        let
          end = do
            Lamdera.debug "[backendSt] 🧠"
            text <- atomically $ readTVar beState
            bePath <- lamderaBackendDevSnapshotPath
            writeUtf8 bePath text

        flip finally end $
         httpServe (config port) $
          serveFiles
          <|> serveDirectoryWith directoryConfig "." -- Default directory/file config
          <|> serveWebsocket mClients mLeader beState mChan
          <|> route [ ("_r/:endpoint", serveRpc mClients mLeader mChan) ]
          <|> serveAssets -- Compiler packaged static files
          <|> serveLamderaPublicFiles -- Add /public/* as if it were /* to mirror production
          <|> serveUnmatchedUrlsToIndex -- Everything else without extensions goes to Lamdera LocalDev harness
          <|> error404 -- Will get hit for any non-matching extensioned paths i.e. /hello.blah


config :: Int -> Config Snap a
config port =
  Snap.Http.Server.defaultConfig
    # setVerbose False
    # setPort port
    # setAccessLog ConfigNoLog
    # setErrorLog ConfigNoLog


(#) :: a -> (a -> b) -> b
(#) value func =
  func value



-- INDEX


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  let
    customGenerator pwd = do
      if (pwd == ".")
        then do
          Lamdera.debug "passing on / index"
          pass
        else
          pure ()

      modifyResponse $ setContentType "text/html; charset=utf-8"
      html <- liftIO $
        do  root <- Dir.getCurrentDirectory
            Index.get root pwd
      writeBuilder html
  in
    fancyDirectoryConfig
      { indexFiles = []
      , indexGenerator = customGenerator
      }



-- NOT FOUND


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ Generate.makePageHtml "NotFound" Nothing



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  file <- getSafePath
      guard =<< liftIO (Dir.doesFileExist file)
      serveElm file <|> serveFilePretty file



-- SERVE FILES + CODE HIGHLIGHTING


serveFilePretty :: FilePath -> Snap ()
serveFilePretty file =
  let
    possibleExtensions =
      getSubExts (takeExtensions file)
  in
    case mconcat (map lookupMimeType possibleExtensions) of
      Nothing ->
        serveCode file

      Just mimeType ->
        serveFileAs mimeType file


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []

  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))


serveCode :: String -> Snap ()
serveCode file =
  do  code <- liftIO (BS.readFile file)
      modifyResponse (setContentType "text/html")
      writeBuilder $
        Generate.makeCodeHtml ('~' : '/' : file) (B.byteString code)



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm file =
  do  guard (takeExtension file == ".elm")
      modifyResponse (setContentType "text/html")
      writeBuilder =<< liftIO (compileToHtmlBuilder file)


compileToHtmlBuilder :: FilePath -> IO B.Builder
compileToHtmlBuilder file =
  do  mvar1 <- newEmptyMVar
      mvar2 <- newEmptyMVar

      let reporter = Progress.Reporter (\_ -> return ()) (\_ -> return True) (putMVar mvar1)
      let output = Just (Output.HtmlBuilder mvar2)

      void $ Task.try reporter $
        do  summary <- Project.getRoot
            Project.compile Output.Dev Output.Client output Nothing summary [file]

      result <- takeMVar mvar1
      case result of
        Just exit ->
          return $ Generate.makePageHtml "Errors" (Just (Exit.toJson exit))

        Nothing ->
          takeMVar mvar2



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  file <- getSafePath
      case StaticFiles.lookup file of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
              -- Lamdera.debug $ "serving assets: " <> file
              writeBS content



-- MIME TYPES


lookupMimeType :: FilePath -> Maybe BS.ByteString
lookupMimeType ext =
  HashMap.lookup ext mimeTypeDict


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)


mimeTypeDict :: HashMap.HashMap FilePath BS.ByteString
mimeTypeDict =
  HashMap.fromList
    [ ".asc"     ==> "text/plain"
    , ".asf"     ==> "video/x-ms-asf"
    , ".asx"     ==> "video/x-ms-asf"
    , ".avi"     ==> "video/x-msvideo"
    , ".bz2"     ==> "application/x-bzip"
    , ".css"     ==> "text/css"
    , ".dtd"     ==> "text/xml"
    , ".dvi"     ==> "application/x-dvi"
    , ".gif"     ==> "image/gif"
    , ".gz"      ==> "application/x-gzip"
    , ".htm"     ==> "text/html"
    , ".html"    ==> "text/html"
    , ".ico"     ==> "image/x-icon"
    , ".jpeg"    ==> "image/jpeg"
    , ".jpg"     ==> "image/jpeg"
    , ".js"      ==> "text/javascript"
    , ".json"    ==> "application/json"
    , ".m3u"     ==> "audio/x-mpegurl"
    , ".mov"     ==> "video/quicktime"
    , ".mp3"     ==> "audio/mpeg"
    , ".mpeg"    ==> "video/mpeg"
    , ".mpg"     ==> "video/mpeg"
    , ".ogg"     ==> "application/ogg"
    , ".pac"     ==> "application/x-ns-proxy-autoconfig"
    , ".pdf"     ==> "application/pdf"
    , ".png"     ==> "image/png"
    , ".qt"      ==> "video/quicktime"
    , ".sig"     ==> "application/pgp-signature"
    , ".spl"     ==> "application/futuresplash"
    , ".svg"     ==> "image/svg+xml"
    , ".swf"     ==> "application/x-shockwave-flash"
    , ".tar"     ==> "application/x-tar"
    , ".tar.bz2" ==> "application/x-bzip-compressed-tar"
    , ".tar.gz"  ==> "application/x-tgz"
    , ".tbz"     ==> "application/x-bzip-compressed-tar"
    , ".text"    ==> "text/plain"
    , ".tgz"     ==> "application/x-tgz"
    , ".ttf"     ==> "application/x-font-truetype"
    , ".txt"     ==> "text/plain"
    , ".wav"     ==> "audio/x-wav"
    , ".wax"     ==> "audio/x-ms-wax"
    , ".wma"     ==> "audio/x-ms-wma"
    , ".wmv"     ==> "video/x-ms-wmv"
    , ".xbm"     ==> "image/x-xbitmap"
    , ".xml"     ==> "text/xml"
    , ".xpm"     ==> "image/x-xpixmap"
    , ".xwd"     ==> "image/x-xwindowdump"
    , ".zip"     ==> "application/zip"
    ]



-- Additional handler to serve files in /public from root / so that
-- image/asset references from Elm work locally same as in production
serveLamderaPublicFiles :: Snap ()
serveLamderaPublicFiles =
  do  file <- getSafePath
      let pubFile = "public" </> file
      guard =<< liftIO (Dir.doesFileExist pubFile)
      -- Lamdera.debug $ "serving lamdera public files: " <> file
      serveElm pubFile <|> serveFilePretty pubFile


-- So that Elm's Navigation routing can work on any URL, serve any unmatched
-- non-extensioned paths to the "index" (in this case the src/LocalDev.elm
-- harness as we're local in the reactor). Extensioned paths will continue to
-- the next handler, namely `error404` (see `run` fn at top of file)
serveUnmatchedUrlsToIndex :: Snap ()
serveUnmatchedUrlsToIndex =
  do  file <- getSafePath
      guard (takeExtension file == "")

      root <- liftIO $ getProjectRoot
      let harnessPath = root </> "lamdera-stuff/alpha/LocalDev.elm"

      isDebug <- liftIO $ Lamdera.isDebug
      harness <-
        if isDebug
          then do
            let overridePath = "/Users/mario/dev/projects/elmx/ui/browser/src/LocalDev.elm"
            overrideExists <- liftIO $ Lamdera.doesFileExist overridePath
            if overrideExists
              then
                liftIO $ BS.readFile ("/Users/mario/dev/projects/elmx/ui/browser/src/LocalDev.elm")
              else
                pure StaticFiles.lamderaLocalDev
          else
            pure StaticFiles.lamderaLocalDev

      liftIO $ Lamdera.mkdir $ root </> "lamdera-stuff/alpha"
      liftIO $ BS.writeFile harnessPath harness
      serveElm harnessPath
      -- liftIO $ Dir.removeFile harnessPath
      -- liftIO $ callCommand $ "rm " <> harnessPath <> " || true " -- less exception-ey on double-reload!



-- serveWebsocket :: TVar [Client] -> Snap ()
serveWebsocket mClients mLeader beState mChan =
  do  file <- getSafePath
      guard (file == "_w")
      mKey <- getHeader "sec-websocket-key" <$> getRequest

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

                leader <- atomically $ readTVar mLeader
                case leader of
                  Just leaderId ->
                    pure $ Just $ "{\"t\":\"s\",\"c\":\"" <> clientId <> "\",\"l\":\"" <> leaderId <> "\"}"

                  Nothing ->
                    -- Impossible
                    pure Nothing

              onReceive clientId text = do
                if Text.isPrefixOf "{\"t\":\"BackendModel\"," text
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

          WS.runWebSocketsSnap $ SocketServer.socketHandler mClients mLeader beState onJoined onReceive (T.decodeUtf8 key)


        Nothing ->
          error404


serveRpc mClients mLeader mChan = do

  mEndpoint <- getParam "endpoint"
  rbody <- readRequestBody 10000000 -- 10MB limit
  mSid <- getCookie "sid"
  contentType <- getHeader "Content-Type" <$> getRequest

  onlyWhen (mSid == Nothing) $ error500 "no sid present"
  onlyWhen (mEndpoint == Nothing) $ error500 "no endpoint present"

  -- Using UUIDv4 here instead of UUIDv1 like in production is merely a matter
  -- of ergonomics; The UUIDv1 package only has `nextUUID :: IO (Maybe UUID)`
  -- as it returns Nothing for requests too close together, so using UUIDv4
  -- was more practical than implementing a UUIDv1 with retry
  reqId <- liftIO $ UUID.toText <$> UUID.nextRandom
  outChan <- newBChanListener mChan

  let
    sid =
      case mSid of
        Just sid_ ->
          T.decodeUtf8 $ cookieValue sid_

        Nothing ->
          -- Should be impossible given we already checked above
          error "no sid present"

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

        Nothing ->
          error "invalid Content-Type"

    loopRead = do
      Lamdera.sleep 100
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
                  Lamdera.debugT $ "😢  rpc response decoding failed for " <> chanText
                  pure $ writeBuilder $ B.byteString $ "rpc response decoding failed for " <> T.encodeUtf8 chanText

            else
              loopRead

  liftIO $ sendToLeader mClients mLeader (\leader -> pure payload)

  result <- liftIO $ timeout 2 $ loopRead

  case result of
    Just builder -> do
      builder
    Nothing -> do
      Lamdera.debugT $ "⏰ RPC timed out for:" <> payload
      writeBuilder "error:timeout"


-- error500 :: Snap ()
error500 s =
  do  modifyResponse $ setResponseStatus 500 "Internal server error"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ "error:" <> s
