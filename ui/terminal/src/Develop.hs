{-# LANGUAGE OverloadedStrings #-}
module Develop
  ( Flags(..)
  , run
  )
  where


import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (guard, void)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
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
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Data.List as List
import System.Process

import Elm.Project.Json
import Elm.Project.Summary

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
    do  putStrLn $ "Go to <http://localhost:" ++ show port ++ "> to run your Lamdera project."

        mClients <- liftIO $ SocketServer.clientsInit

        forkIO $ withManager $ \mgr -> do
          -- start a watching job (in the background)
          watchTree
            mgr          -- manager
            "."          -- directory to watch
            (const True) -- predicate
            (\e -> do
              let
                shouldRefresh =
                  case e of
                    Modified filename _ _ ->
                      not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)
                    Removed filename _ _ ->
                      not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)
                    _ ->
                      True

              if shouldRefresh
                then do
                  Lamdera.debug $ "refreshing for change: " <> show e
                  SocketServer.broadcastImpl mClients "r" -- r is refresh, see live.js
                else
                  pure ()
            )  -- action

          -- sleep forever (until interrupted)
          forever $ threadDelay 1000000

        httpServe (config port) $
          serveFiles
          <|> serveDirectoryWith directoryConfig "." -- Default directory/file config
          <|> serveWebsocket mClients
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
      let harnessPath = "lamdera-stuff/alpha/LocalDev.elm"
      -- Lamdera.debug $ "serving unmatched URL: " <> file

      d <- liftIO $ Lamdera.isDebug
      harness <-
        if d then
          liftIO $ BS.readFile ("/Users/mario/dev/projects/elmx/ui/browser/src/LocalDev.elm")
        else
          pure StaticFiles.lamderaLocalDev

      liftIO $ callCommand $ "mkdir -p lamdera-stuff/alpha"
      liftIO $ BS.writeFile harnessPath harness
      serveElm harnessPath
      -- liftIO $ Dir.removeFile harnessPath
      -- liftIO $ callCommand $ "rm " <> harnessPath <> " || true " -- less exception-ey on double-reload!



-- serveWebsocket :: TVar [Client] -> Snap ()
serveWebsocket mClients =
  do  file <- getSafePath
      guard (file == "_w")

      mKey <- getHeader "sec-websocket-key" <$> getRequest

      case mKey of
        Just key -> do

          let onJoined _ _ = pure Nothing
              onReceive _ text = do
                Lamdera.debugT $ "onReceive:" <> text

          WS.runWebSocketsSnap $ SocketServer.socketHandler mClients onJoined onReceive (T.decodeUtf8 key)

        Nothing ->
          error404
