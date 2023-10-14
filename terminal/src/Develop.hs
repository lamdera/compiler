{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Develop
  ( Flags(..)
  , run
  , runWithRoot
  )
  where


import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
-- import qualified Control.Monad.Catch
import qualified Control.Exception.Lifted
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import System.FilePath as FP
import Snap.Core hiding (path)
import qualified Snap.Core as SnapCore
import Snap.Http.Server
import Snap.Util.FileServe

import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Develop.Generate.Help as Help
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Generate.Html as Html
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff


import Lamdera
import qualified Lamdera.CLI.Live as Live
import qualified Lamdera.Constrain
import qualified Lamdera.ReverseProxy
import qualified Lamdera.TypeHash
import qualified Lamdera.PostCompile

import Ext.Common (trackedForkIO, whenDebug)
import qualified Ext.Filewatch as Filewatch
import qualified Ext.Sentry as Sentry
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)

import StandaloneInstances

import qualified Artifacts
import qualified Endpoint.Repl as Repl
import qualified Endpoint.Package as Package

import Data.IORef

-- RUN THE DEV SERVER


data Flags =
  Flags
    { _port :: Maybe Int
    }


run :: () -> Flags -> IO ()
run () flags = do
  root <- getProjectRoot "Develop.run"
  runWithRoot root flags

-- currentArtifacts <- liftIO $ readIORef artifactRef

runWithRoot :: FilePath -> Flags -> IO ()
runWithRoot root (Flags maybePort) =
  do
      Lamdera.setLiveMode True
      let port = maybe 8000 id maybePort
      liftIO $ Lamdera.stdoutSetup
      atomicPutStrLn $ "Go to http://localhost:" ++ show port ++ " to see your project dashboard."

      Lamdera.setProjectRoot root

      onlyWhen (port == 8001) $
        error "Port 8001 is reserved for the Lamdera proxy, please pick another port."

      liveState <- liftIO $ Live.init

      sentryCache <- liftIO $ Sentry.init

      initialArtifacts <- Artifacts.loadRepl
      artifactRef <- newIORef initialArtifacts


      let
        recompile :: [String] -> IO ()
        recompile events = do
          -- Fork a recompile+cache update
          Sentry.asyncUpdateJsOutput sentryCache $ do
            debug_ $ "🛫  recompile triggered by: " ++ show events
            harness <- Live.prepareLocalDev root
            let
              typesRootChanged = events & filter (\event ->
                     stringContains "src/Types.elm" event
                  || stringContains "src/Bridge.elm" event
                ) & length & (\v -> v > 0)
            onlyWhen typesRootChanged Lamdera.Constrain.resetTypeLocations
            compileToBuilder harness
          -- Simultaneously tell all clients to refresh. All those HTTP
          -- requests will open but block on the cache mVar, and then release
          -- immediately when compilation is finished, effectively "pushing"
          -- the result to waiting browsers without paying the TCP+HTTP cost again
          Live.refreshClients liveState

      -- Warm the cache
      recompile []

      Filewatch.watch root recompile

      whenDebug $ do
        -- Watch LocalDev.elm changes when in Debug mode to assist with development
        home <- Dir.getHomeDirectory
        let override = home <> "/dev/projects/lamdera-compiler/extra/LocalDev/LocalDev.elm"
        onlyWhen_ (doesFileExist override) $ do
          Filewatch.watchFile override recompile

      Lamdera.ReverseProxy.start

      -- rArtifacts <- Artifacts.loadRepl
      initialArtifacts <- Artifacts.loadRepl
      artifactRef <- newIORef initialArtifacts

      Live.withEnd liveState $
       httpServe (config port) $ gcatchlog "general" $
        -- Add /public/* as if it were /* to mirror production, but still render .elm files as an Elm app first
        Live.serveLamderaPublicFiles root (serveElm sentryCache)
        <|> (serveFiles root sentryCache)
        <|> serveDirectoryWith directoryConfig "."
        <|> Live.serveWebsocket root liveState
        <|> route [ ("_r/:endpoint", Live.serveRpc liveState port) ]
        <|> Live.openEditorHandler root
        <|> Live.serveExperimental root
        <|> (SnapCore.path "repl" $ Repl.endpoint artifactRef)
        <|> (SnapCore.path "packageList" $ Package.handlePost artifactRef)
        <|> (SnapCore.path "reportOnInstalledPackages" $ Package.reportOnInstalledPackages)
        <|> serveAssets -- Compiler packaged static files
        <|> Live.serveUnmatchedUrlsToIndex root (serveElm sentryCache) -- Everything else without extensions goes to Lamdera LocalDev harness
        <|> error404 -- Will get hit for any non-matching extensioned paths i.e. /hello.blah


-- Try narrow down source of exceptions with generalised error catch
gcatchlog :: String -> Snap () -> Snap ()
gcatchlog tag snap =
  snap `Control.Exception.Lifted.catch` (\(e :: Control.Exception.Lifted.SomeException) -> do
    liftIO $ atomicPutStrLn $ "🚨  gcatchlog: " ++ show e
    pure ()
  )
  -- where
  --   bar = throwIO FooException

  -- snap `Control.Exception.Lifted.catch` (\e ->
  --   pure ()
  -- )



config :: Int -> Config Snap a
config port =
  setVerbose True $ setPort port $
    -- setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig
    -- Unsure why errors aren't logged in original impl, exceptions get eaten otherwise...
    setAccessLog (ConfigIoLog Live.logger) $ setErrorLog (ConfigIoLog Live.logger) $ defaultConfig


-- INDEX


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  fancyDirectoryConfig
    { indexFiles = []
    , indexGenerator = \pwd ->
        do  Live.passOnIndex pwd
            modifyResponse $ setContentType "text/html;charset=utf-8"
            writeBuilder =<< liftIO (Index.generate pwd)
    }



-- NOT FOUND


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html;charset=utf-8"
      writeBuilder $ Help.makePageHtml "NotFound" Nothing



-- SERVE FILES


serveFiles :: FilePath -> Sentry.Cache -> Snap ()
serveFiles root sentryCache =
  do  path <- getSafePath
      guard =<< liftIO (Dir.doesFileExist (root </> path))
      serveElm_ root path <|> serveFilePretty (root </> path)



-- SERVE FILES + CODE HIGHLIGHTING


serveFilePretty :: FilePath -> Snap ()
serveFilePretty path =
  let
    possibleExtensions =
      getSubExts (takeExtensions path)
  in
    case mconcat (map lookupMimeType possibleExtensions) of
      Nothing ->
        serveCode path

      Just mimeType ->
        serveFileAs mimeType path


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []
  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))


serveCode :: String -> Snap ()
serveCode path =
  do  code <- liftIO (BS.readFile path)
      modifyResponse (setContentType "text/html")
      writeBuilder $
        Help.makeCodeHtml ('~' : '/' : path) (B.byteString code)



-- SERVE ELM

serveElm :: Sentry.Cache -> FilePath -> Snap ()
serveElm sentryCache path =
  do  guard (takeExtension path == ".elm")
      modifyResponse (setContentType "text/html")
      js <- liftIO $ Sentry.getJsOutput sentryCache
      writeBS js


compileToBuilder :: FilePath -> IO BS.ByteString
compileToBuilder path =
  do
      result <- compile path

      pure $
        BSL.toStrict $
          B.toLazyByteString $
            case result of
                Right builder ->
                  builder

                Left exit -> do
                  -- @LAMDERA because we do AST injection, sometimes we might get
                  -- an error that actually cannot be displayed, i.e, the reactorToReport
                  -- function itself throws an exception, mainly due to use of unsafe
                  -- functions like Prelude.last and invariants that for some reason haven't
                  -- held with our generated code (usually related to subsequent type inference)
                  -- We print out a less-processed version here in debug mode to aid with
                  -- debugging in these scenarios, as the browser will just get zero bytes
                  -- debugPass "serveElm error" (Exit.reactorToReport exit) (pure ())
                  Help.makePageHtml "Errors" $ Just $
                    Exit.toJson $ Exit.reactorToReport exit


serveElm_ :: FilePath -> FilePath -> Snap ()
serveElm_ root path =
  do  guard (takeExtension path == ".elm")
      modifyResponse (setContentType "text/html")
      liftIO $ atomicPutStrLn $ "⛑  manually compiling: " <> path

      result <- liftIO $ compile (root </> path)
      case result of
        Right builder ->
          writeBuilder builder

        Left exit -> do
          -- @LAMDERA because we do AST injection, sometimes we might get
          -- an error that actually cannot be displayed, i.e, the reactorToReport
          -- function itself throws an exception, mainly due to use of unsafe
          -- functions like Prelude.last and invariants that for some reason haven't
          -- held with our generated code (usually related to subsequent type inference)
          -- We print out a less-processed version here in debug mode to aid with
          -- debugging in these scenarios, as the browser will just get zero bytes
          debugPass "serveElm error" (Exit.reactorToReport exit) (pure ())
          writeBuilder $ Help.makePageHtml "Errors" $ Just $
            Exit.toJson $ Exit.reactorToReport exit


compile :: FilePath -> IO (Either Exit.Reactor B.Builder)
compile path =
  do  maybeRoot <- Stuff.findRootHelp $ FP.splitDirectories $ FP.takeDirectory path
      case maybeRoot of
        Nothing ->
          return $ Left $ Exit.ReactorNoOutline

        Just root ->
          BW.withScope $ \scope -> Stuff.withRootLock root $ Task.run $
            do  details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
                artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details (NE.List path [])

                Lamdera.PostCompile.check details artifacts Exit.ReactorBadBuild
                Lamdera.TypeHash.buildCheckHashes artifacts

                javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
                let (NE.List name _) = Build.getRootNames artifacts
                return $ Html.sandwich root name javascript



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  path <- getSafePath
      case StaticFiles.lookup path of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
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
    , ".mp4"     ==> "video/mp4"
    , ".mpeg"    ==> "video/mpeg"
    , ".mpg"     ==> "video/mpeg"
    , ".ogg"     ==> "application/ogg"
    , ".otf"     ==> "font/otf"
    , ".pac"     ==> "application/x-ns-proxy-autoconfig"
    , ".pdf"     ==> "application/pdf"
    , ".png"     ==> "image/png"
    , ".qt"      ==> "video/quicktime"
    , ".sfnt"    ==> "font/sfnt"
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
    , ".ttf"     ==> "font/ttf"
    , ".txt"     ==> "text/plain"
    , ".wav"     ==> "audio/x-wav"
    , ".wax"     ==> "audio/x-ms-wax"
    , ".webm"    ==> "video/webm"
    , ".webp"    ==> "image/webp"
    , ".wma"     ==> "audio/x-ms-wma"
    , ".wmv"     ==> "video/x-ms-wmv"
    , ".woff"    ==> "font/woff"
    , ".woff2"   ==> "font/woff2"
    , ".xbm"     ==> "image/x-xbitmap"
    , ".xml"     ==> "text/xml"
    , ".xpm"     ==> "image/x-xpixmap"
    , ".xwd"     ==> "image/x-xwindowdump"
    , ".zip"     ==> "application/zip"
    ]
