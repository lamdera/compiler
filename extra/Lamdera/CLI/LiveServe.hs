{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.LiveServe
  ( Share(..)
  , Target(..)
  , prepare
  , withTunnel
  , localTarget
    -- for tests
  , FrameType(..)
  , Frame(..)
  , encodeFrame
  , decodeFrame
  , windowPayload
  , windowIncrement
  , RelayUrl(..)
  , parseRelayUrl
  , defaultRelay
  , helloJson
  , machineSecretPath
  , machineSecret
  , ServerMessage(..)
  , parseServerMessage
  , backoffSeconds
  , initialWindow
  , maxChunk
  ) where

{- `lamdera live --serve`: share the `lamdera live` session at a public
https://<app>-<random>.lamdera.dev, for as long as it runs.

The CLI keeps one WebSocket to the live relay (wss://connect.lamdera.dev/tunnel,
LAMDERA_LIVE_RELAY overrides it), which answers the preview's address. Every request
to that address (a page, an RPC, the /_w socket) becomes a stream on that WebSocket:
the relay opens it, and the CLI connects it to this `lamdera live` server and copies
the bytes both ways. The request reaches `lamdera live` exactly as sent, Host included,
so it is not local (Lamdera.CLI.LocalOnly): the REPL, the file tools and the project's
own files stay on this computer, and the visitor gets the app.

The protocol (docs/plans/cli-app-management.md in the platform repo, and the relay in
nixos/live-relay/relay.js there):

  Text messages are JSON, for control:

    CLI -> relay  {"t":"hello","v":1,"token":..,"app":..,"client":..,
                   "resume":<host>?,"resumeKey":..?}             the first message
    relay -> CLI  {"t":"ready","url":..,"host":..,"resumeKey":..}  shared at url
                  {"t":"error","message":..}                       refused, then closed
                  {"t":"notice","message":..}                      for the developer

  Binary messages are stream frames: a type byte, the stream id (4 bytes, big endian),
  and the payload.

    1 OPEN    relay -> CLI   a new request: connect to the local server
    2 DATA    both           bytes of the stream
    3 END     both           no more bytes from this side
    4 RESET   both           the stream is gone (payload: a reason, for logs)
    5 WINDOW  both           the payload (4 bytes) is how many more bytes of DATA
                             the other side may send on this stream

  Each side may send initialWindow bytes of DATA on a stream before it gets a
  WINDOW: the receiver hands one out for every chunk it has written on, so neither
  side ever holds more than a window per stream in memory.
-}

import Control.Concurrent (forkIO, threadDelay, killThread, myThreadId, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, AsyncException(..), try, catch, finally, bracket, throwIO, throwTo, Exception, fromException)
import Control.Monad (forM_, forever, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.POSIX as Time
import Data.Word (Word8, Word32)
import qualified Network.Connection as Connection
import qualified Data.Default as Default
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream
import System.Exit (exitFailure)
import qualified System.Environment as Env
import qualified System.Process
import System.Exit (ExitCode(..))
import Control.Monad.Except (catchError)
import Text.Read (readMaybe)

import qualified Reporting.Doc as D
import qualified Root
import qualified Data.Char as Char
import qualified System.Directory as Dir
import qualified System.Entropy as Entropy
import qualified System.FilePath as FP
import System.FilePath ((</>))
import qualified System.PosixCompat.Files as Files

import Lamdera (debug_)
import qualified Lamdera.CLI.Login as Login
import qualified Lamdera.Http
import qualified Lamdera.Progress as Progress
import qualified Lamdera.Project
import qualified Lamdera.Version



-- WHAT IS SHARED


data Share =
  Share
    { _shareApp :: T.Text
    , _shareToken :: T.Text
    , _shareRelay :: RelayUrl
    , _shareMachine :: T.Text -- this computer's secret for the app, see machineSecret
    }


{-| Where `lamdera live` listens: the tunnel connects each stream there. -}
data Target =
  Target
    { _targetHost :: String
    , _targetPort :: Int
    }
    deriving (Show, Eq)


localTarget :: String -> Int -> Target
localTarget bind port =
  Target
    (case bind of
      "0.0.0.0" -> "127.0.0.1"
      "" -> "127.0.0.1"
      "::" -> "::1"
      other -> other
    )
    port


{-| The app and the CLI login --serve needs, or what to do about them reported and exit,
before `lamdera live` starts. With `new` (--serve-new), the app gets a new address. -}
prepare :: Maybe String -> Bool -> IO Share
prepare appM new = do
  appNameM <-
    case appM of
      Just name -> pure (Just (T.pack name))
      Nothing -> do
        -- maybeAppName asks git, which complains on stderr outside a repository
        (exit, _, _) <- System.Process.readProcessWithExitCode "git" ["rev-parse", "--is-inside-work-tree"] ""
          `catchError` (\_ -> pure (ExitFailure 1, "", ""))
        if exit == ExitSuccess then Lamdera.Project.maybeAppName else pure Nothing
  appName <-
    case appNameM of
      Just name -> pure name
      Nothing ->
        failWith
          "`lamdera live --serve` shares your app at https://<app>-<random>.lamdera.dev, so it needs to know which app this is: pass --app NAME, or run it in a Lamdera project with a `lamdera` git remote."
  tokenR <- Login.checkCliToken (Just appName)
  token <-
    case tokenR of
      Right token -> pure token
      Left Login.CliAuthNoToken ->
        failWith "`lamdera live --serve` needs you to be logged in: run `lamdera login` first."
      Left Login.CliAuthInvalid ->
        failWith "Your CLI login has expired or is invalid: run `lamdera login` again, then `lamdera live --serve`."
      Left (Login.CliAuthHttp err) -> do
        Lamdera.Http.printHttpError err "I need to check your CLI login before sharing the app"
        exitFailure
  relayM <- Env.lookupEnv "LAMDERA_LIVE_RELAY"
  relay <-
    case maybe (Right defaultRelay) parseRelayUrl (mfilter_ relayM) of
      Right r -> pure r
      Left problem -> failWith ("LAMDERA_LIVE_RELAY: " <> T.pack problem)
  elmHome <- Root.getElmHomePath
  machine <- machineSecret (machineSecretPath elmHome appName) new
  when new $
    Progress.report $ D.fillSep $
      [ "───>", D.yellow "New preview address" ]
        ++ fmap D.fromChars (words ("for " <> T.unpack appName <> " (--serve-new): the previous link stops working. The new one stays the same each time you share " <> T.unpack appName <> " from this computer."))
  pure (Share appName token relay machine)
  where
    mfilter_ m = case m of { Just s | not (null s) -> Just s; _ -> Nothing }


{-| Where this computer keeps its secret for sharing an app: the relay derives the
preview's name from it (with the user and the app), so the address is the same every
time the app is shared from here. -}
machineSecretPath :: FilePath -> T.Text -> FilePath
machineSecretPath elmHome appName =
  elmHome </> ".lamdera-live" </> T.unpack appName


{-| The secret at `path`, made the first time, or new when `rotate` (--serve-new). 48 hex
characters, readable only by its owner. -}
machineSecret :: FilePath -> Bool -> IO T.Text
machineSecret path rotate = do
  existing <-
    if rotate
      then pure Nothing
      else do
        exists <- Dir.doesFileExist path
        if exists then Just . T.strip . TE.decodeUtf8 <$> BS.readFile path else pure Nothing
  case existing of
    Just secret | T.length secret >= 16 && T.all (\c -> Char.isAsciiLower c || Char.isDigit c) secret ->
      pure secret
    _ -> do
      bytes <- Entropy.getEntropy 24
      let secret = T.pack (concatMap hex (BS.unpack bytes))
      Dir.createDirectoryIfMissing True (FP.takeDirectory path)
      BS.writeFile path (TE.encodeUtf8 secret)
      Files.setFileMode path (Files.ownerReadMode `Files.unionFileModes` Files.ownerWriteMode)
      pure secret
  where
    hex w = let (a, b) = w `divMod` 16 in [ Char.intToDigit (fromIntegral a), Char.intToDigit (fromIntegral b) ]


failWith :: T.Text -> IO a
failWith message = do
  Progress.report $ D.fillSep ([ "───>", D.red "Error:" ] ++ fmap (D.fromChars . T.unpack) (T.words message))
  exitFailure



-- RELAY URL


data RelayUrl =
  RelayUrl
    { _relaySecure :: Bool
    , _relayHost :: String
    , _relayPort :: Int
    , _relayPath :: String
    }
    deriving (Show, Eq)


defaultRelay :: RelayUrl
defaultRelay =
  RelayUrl True "connect.lamdera.dev" 443 "/tunnel"


parseRelayUrl :: String -> Either String RelayUrl
parseRelayUrl url =
  let
    (secure, rest) =
      case break (== ':') url of
        ("wss", ':' : '/' : '/' : r) -> (Just True, r)
        ("ws", ':' : '/' : '/' : r) -> (Just False, r)
        _ -> (Nothing, url)
    (authority, path) = break (== '/') rest
    (host, portPart) =
      case authority of
        '[' : v6 -> let (h, p) = break (== ']') v6 in (h, drop 1 p)
        _ -> break (== ':') authority
  in
  case secure of
    Nothing -> Left ("expected a ws:// or wss:// URL, got " <> url)
    Just s ->
      if null host
        then Left ("no host in " <> url)
        else
          case portPart of
            "" -> Right (RelayUrl s host (if s then 443 else 80) (if null path then "/" else path))
            ':' : p | Just n <- readMaybe p -> Right (RelayUrl s host n (if null path then "/" else path))
            _ -> Left ("bad port in " <> url)



-- FRAMES


data FrameType
  = Open
  | Data
  | End
  | Reset
  | Window
  deriving (Show, Eq, Enum, Bounded)


data Frame =
  Frame
    { _frameType :: FrameType
    , _frameStream :: Word32
    , _framePayload :: BS.ByteString
    }
    deriving (Show, Eq)


frameCode :: FrameType -> Word8
frameCode t =
  case t of
    Open -> 1
    Data -> 2
    End -> 3
    Reset -> 4
    Window -> 5


encodeFrame :: Frame -> BSL.ByteString
encodeFrame (Frame t stream payload) =
  Put.runPut $ do
    Put.putWord8 (frameCode t)
    Put.putWord32be stream
    Put.putByteString payload


decodeFrame :: BSL.ByteString -> Either String Frame
decodeFrame bytes =
  case Get.runGetOrFail getFrame bytes of
    Right (_, _, frame) -> Right frame
    Left (_, _, problem) -> Left problem
  where
    getFrame = do
      code <- Get.getWord8
      stream <- Get.getWord32be
      payload <- BSL.toStrict <$> Get.getRemainingLazyByteString
      case lookup code [ (frameCode t, t) | t <- [minBound .. maxBound] ] of
        Just t -> pure (Frame t stream payload)
        Nothing -> fail ("unknown frame type " <> show code)


windowPayload :: Int -> BS.ByteString
windowPayload n =
  BSL.toStrict $ Put.runPut $ Put.putWord32be (fromIntegral n)


windowIncrement :: BS.ByteString -> Maybe Int
windowIncrement payload =
  case Get.runGetOrFail Get.getWord32be (BSL.fromStrict payload) of
    Right (rest, _, n) | BSL.null rest -> Just (fromIntegral n)
    _ -> Nothing


{-| How much DATA a side may send on a stream before it hears a WINDOW. -}
initialWindow :: Int
initialWindow =
  256 * 1024


{-| The largest DATA payload sent. -}
maxChunk :: Int
maxChunk =
  32 * 1024



-- CONTROL MESSAGES


helloJson :: T.Text -> T.Text -> T.Text -> Maybe (T.Text, T.Text) -> BSL.ByteString
helloJson token app machine resumeM =
  Aeson.encode $ Aeson.object $
    [ "t" Aeson..= ("hello" :: T.Text)
    , "v" Aeson..= (1 :: Int)
    , "token" Aeson..= token
    , "app" Aeson..= app
    , "machine" Aeson..= machine
    , "client" Aeson..= T.pack ("lamdera " <> Lamdera.Version.short)
    ]
    ++ case resumeM of
        Just (host, key) -> [ "resume" Aeson..= host, "resumeKey" Aeson..= key ]
        Nothing -> []


data ServerMessage
  = Ready T.Text T.Text T.Text -- url, host, resumeKey
  | Refused T.Text
  | Notice T.Text
  | Unknown T.Text
  deriving (Show, Eq)


parseServerMessage :: BSL.ByteString -> ServerMessage
parseServerMessage bytes =
  case Aeson.decode bytes of
    Just (Aeson.Object o) ->
      let
        text k = case KeyMap.lookup (Key.fromText k) o of { Just (Aeson.String s) -> Just s; _ -> Nothing }
      in
      case text "t" of
        Just "ready" | Just url <- text "url", Just host <- text "host" ->
          Ready url host (maybe "" id (text "resumeKey"))
        Just "error" -> Refused (maybe "the live relay refused the tunnel" id (text "message"))
        Just "notice" -> Notice (maybe "" id (text "message"))
        _ -> Unknown (TE.decodeUtf8With (\_ _ -> Just '?') (BSL.toStrict bytes))
    _ ->
      Unknown (TE.decodeUtf8With (\_ _ -> Just '?') (BSL.toStrict bytes))


{-| Seconds to wait before reconnect attempt n (0-based): 1, 2, 4, 8, 16, then 30. -}
backoffSeconds :: Int -> Int
backoffSeconds attempt =
  min 30 (2 ^ min 5 attempt)



-- THE TUNNEL


data Outcome
  = Shared T.Text
  | Fatal T.Text
  | Unreachable T.Text


{-| Runs `io` (the `lamdera live` server) with the tunnel up beside it. Waits for the
relay's first answer, so a refusal (an app this login can't use, too many previews)
stops `lamdera live` with the reason instead of scrolling past. When `io` ends, the
tunnel is closed, and the preview with it. -}
withTunnel :: Share -> Target -> IO () -> IO ()
withTunnel share target io = do
  first <- newEmptyMVar
  current <- newTVarIO Nothing
  stopping <- newTVarIO False
  tid <- forkIO $ tunnelLoop share target current stopping (\o -> void (tryPutMVar first o))
  outcome <- readMVar first
  case outcome of
    Fatal message -> do
      killThread tid
      failWith message
    Unreachable problem ->
      Progress.report $ D.fillSep $
        [ "───>", D.yellow "Could not reach the live relay" ] ++ fmap (D.fromChars . T.unpack) (T.words ("(" <> problem <> "), retrying in the background. The app runs locally meanwhile."))
    Shared _ ->
      pure ()
  io `finally` do
    atomically $ writeTVar stopping True
    connM <- readTVarIO current
    forM_ connM $ \conn ->
      void (try (WS.sendClose conn ("bye" :: T.Text)) :: IO (Either SomeException ()))
    -- A moment for the close to leave, so the relay drops the preview now rather
    -- than when it notices the connection is gone.
    threadDelay 300000
    killThread tid


tunnelLoop :: Share -> Target -> TVar (Maybe WS.Connection) -> TVar Bool -> (Outcome -> IO ()) -> IO ()
tunnelLoop share target current stopping reportFirst = do
  sharedRef <- newIORef Nothing -- (url, host, resumeKey) of the preview, once shared
  let
    loop attempt = do
      resumeM <- fmap (\(_, host, key) -> (host, key)) <$> readIORef sharedRef
      lastSeen <- newIORef =<< Time.getPOSIXTime
      result <- try $ connect (_shareRelay share) (Time.getPOSIXTime >>= writeIORef lastSeen) $ \conn -> do
        atomically $ writeTVar current (Just conn)
        session share target resumeM lastSeen conn $ \url host key -> do
          previous <- readIORef sharedRef
          writeIORef sharedRef (Just (url, host, key))
          reportFirst (Shared url)
          announce previous url
      atomically $ writeTVar current Nothing
      case result of
        Right (SessionRefused message) -> do
          -- The first time round, withTunnel reports it and stops lamdera live.
          shared <- readIORef sharedRef
          reportFirst (Fatal message)
          when (shared /= Nothing) $
            Progress.report $ D.fillSep ([ "───>", D.red "Stopped sharing:" ] ++ fmap (D.fromChars . T.unpack) (T.words message))
        Right SessionClosed ->
          reconnect 0
        Right SessionEstablishedThenLost ->
          reconnect 0
        Left (e :: SomeException) ->
          case fromException e of
            Just ThreadKilled -> pure ()
            _ -> do
              reportFirst (Unreachable (T.pack (shortError e)))
              debug_ $ "live --serve: " <> show e
              reconnect (attempt + 1)
      where
        reconnect next = do
          shared <- readIORef sharedRef
          stopped <- readTVarIO stopping
          when stopped $ throwIO ThreadKilled -- lamdera live is stopping, and closed the tunnel itself
          when (next == 0 && shared /= Nothing) $
            Progress.report $ D.fillSep [ "───>", D.yellow "Lost the connection to the live relay, reconnecting..." ]
          threadDelay (backoffSeconds next * 1000000)
          loop next
  loop 0


announce :: Maybe (T.Text, T.Text, T.Text) -> T.Text -> IO ()
announce previous url =
  case previous of
    Nothing ->
      Progress.report $ D.vcat
        [ D.fillSep [ "───>", D.green "Sharing at", D.green (D.fromChars (T.unpack url)) ]
        , D.reflow "     Anyone with this link can use the app, sharing its backend with your own browser tabs, after a notice that it runs on your computer. The dev bar, the REPL and the project's files stay on this computer. The link works while `lamdera live` runs, and is the same each time you share this app from this computer (`lamdera live --serve-new` for a new one)."
        ]
    Just (oldUrl, _, _) | oldUrl == url ->
      Progress.report $ D.fillSep [ "───>", D.green "Reconnected, still sharing at", D.green (D.fromChars (T.unpack url)) ]
    Just _ ->
      Progress.report $ D.fillSep [ "───>", D.yellow "Reconnected, the previous link expired. Now sharing at", D.green (D.fromChars (T.unpack url)) ]


shortError :: SomeException -> String
shortError e =
  take 200 (takeWhile (/= '\n') (show e))


connect :: RelayUrl -> IO () -> (WS.Connection -> IO a) -> IO a
connect relay onPong app = do
  let
    headers = [ ("User-Agent", BS8.pack ("lamdera-" <> Lamdera.Version.short)) ]
    options = WS.defaultConnectionOptions { WS.connectionOnPong = onPong }
  if _relaySecure relay
    then do
      context <- Connection.initConnectionContext
      bracket
        (Connection.connectTo context
          (Connection.ConnectionParams
            { Connection.connectionHostname = _relayHost relay
            , Connection.connectionPort = fromIntegral (_relayPort relay)
            , Connection.connectionUseSecure = Just Default.def -- certificates checked against the system's CAs
            , Connection.connectionUseSocks = Nothing
            }
          )
        )
        Connection.connectionClose
        (\connection -> do
          stream <-
            WSStream.makeStream
              (fmap Just (Connection.connectionGetChunk connection))
              (maybe (pure ()) (Connection.connectionPut connection . BSL.toStrict))
          WS.runClientWithStream stream (_relayHost relay) (_relayPath relay) options headers app
        )
    else
      WS.runClientWith (_relayHost relay) (_relayPort relay) (_relayPath relay) options headers app



-- A SESSION: one connection to the relay


data SessionEnd
  = SessionRefused T.Text
  | SessionClosed
  | SessionEstablishedThenLost


data Stream =
  Stream
    { _streamSocket :: TMVar (Maybe NS.Socket) -- Nothing: could not connect
    , _streamIncoming :: TQueue (Maybe BS.ByteString) -- Nothing: END
    , _streamWindow :: TVar Int -- how much DATA we may still send
    , _streamThreads :: TVar [ThreadId]
    }


data Lost = Lost String deriving Show
instance Exception Lost


session :: Share -> Target -> Maybe (T.Text, T.Text) -> IORef Time.POSIXTime -> WS.Connection -> (T.Text -> T.Text -> T.Text -> IO ()) -> IO SessionEnd
session share target resumeM lastSeen conn onReady = do
  WS.sendTextData conn (helloJson (_shareToken share) (_shareApp share) (_shareMachine share) resumeM)
  first <- WS.receiveDataMessage conn
  case first of
    WS.Text bytes _ ->
      case parseServerMessage bytes of
        Ready url host key -> do
          onReady url host key
          serve
        Refused message ->
          pure (SessionRefused message)
        other -> do
          debug_ $ "live --serve: unexpected first message " <> show other
          pure SessionClosed
    WS.Binary _ ->
      pure SessionClosed
  where
    serve = do
      outgoing <- newTBQueueIO 256
      streams <- newTVarIO Map.empty
      me <- myThreadId
      let
        -- Everything goes out through one writer thread, in order.
        send :: WS.Message -> IO ()
        send = atomically . writeTBQueue outgoing

        sendFrame t stream payload =
          send (WS.DataMessage False False False (WS.Binary (encodeFrame (Frame t stream payload))))

        writer = forever $ do
          message <- atomically $ readTBQueue outgoing
          WS.send conn message

        -- WebSocket pings every 15s; the relay answers each with a pong. Nothing heard
        -- for 45s: the connection is gone, even if TCP has not noticed.
        watchdog = forever $ do
          threadDelay 15000000
          send (WS.ControlMessage (WS.Ping ""))
          now <- Time.getPOSIXTime
          seen <- readIORef lastSeen
          when (now - seen > 45) $ throwTo me (Lost "the live relay stopped answering")

        closeStream streamId = do
          entry <- atomically $ do
            m <- readTVar streams
            writeTVar streams (Map.delete streamId m)
            pure (Map.lookup streamId m)
          forM_ entry $ \s -> do
            sockM <- atomically $ tryReadTMVar (_streamSocket s)
            forM_ sockM $ \sock -> forM_ sock $ \sk -> void (try (NS.close sk) :: IO (Either SomeException ()))
            threads <- readTVarIO (_streamThreads s)
            me_ <- myThreadId
            forM_ (filter (/= me_) threads) killThread

        openStream streamId = do
          s <- Stream <$> newEmptyTMVarIO <*> newTQueueIO <*> newTVarIO initialWindow <*> newTVarIO []
          atomically $ modifyTVar' streams (Map.insert streamId s)
          t <- forkQuiet $ do
            sockM <- connectLocal target
            atomically $ putTMVar (_streamSocket s) sockM
            case sockM of
              Nothing -> do
                sendFrame Reset streamId "lamdera live is not answering on this computer"
                closeStream streamId
              Just sock -> do
                doneOut <- newEmptyMVar
                doneIn <- newEmptyMVar
                -- local server -> relay, within the window
                tOut <- forkQuiet $ (do
                  let
                    pump = do
                      chunk <- NSB.recv sock maxChunk
                      if BS.null chunk
                        then sendFrame End streamId ""
                        else do
                          sendWithin chunk
                          pump
                    sendWithin chunk =
                      unless (BS.null chunk) $ do
                        n <- atomically $ do
                          w <- readTVar (_streamWindow s)
                          when (w <= 0) retry
                          let n = min w (BS.length chunk)
                          writeTVar (_streamWindow s) (w - n)
                          pure n
                        sendFrame Data streamId (BS.take n chunk)
                        sendWithin (BS.drop n chunk)
                  pump) `finally` putMVar doneOut ()
                -- relay -> local server, handing out window as it is written
                tIn <- forkQuiet $ (do
                  let
                    drain = do
                      next <- atomically $ readTQueue (_streamIncoming s)
                      case next of
                        Just chunk -> do
                          NSB.sendAll sock chunk
                          sendFrame Window streamId (windowPayload (BS.length chunk))
                          drain
                        Nothing ->
                          void (try (NS.shutdown sock NS.ShutdownSend) :: IO (Either SomeException ()))
                  drain) `finally` putMVar doneIn ()
                atomically $ modifyTVar' (_streamThreads s) ([tOut, tIn] ++)
                -- Done when the local server has finished answering; what the relay
                -- still sends after that has nowhere to go.
                takeMVar doneOut
                closeStream streamId
          atomically $ modifyTVar' (_streamThreads s) (t :)

        dispatch frame =
          case frame of
            Frame Open streamId _ -> openStream streamId
            Frame Data streamId payload -> do
              m <- readTVarIO streams
              case Map.lookup streamId m of
                Just s -> atomically $ writeTQueue (_streamIncoming s) (Just payload)
                Nothing -> sendFrame Reset streamId "no such stream"
            Frame End streamId _ -> do
              m <- readTVarIO streams
              forM_ (Map.lookup streamId m) $ \s -> atomically $ writeTQueue (_streamIncoming s) Nothing
            Frame Reset streamId _ ->
              closeStream streamId
            Frame Window streamId payload -> do
              m <- readTVarIO streams
              case (Map.lookup streamId m, windowIncrement payload) of
                (Just s, Just n) -> atomically $ modifyTVar' (_streamWindow s) (+ n)
                _ -> pure ()

        receiveLoop = forever $ do
          message <- WS.receiveDataMessage conn
          writeIORef lastSeen =<< Time.getPOSIXTime
          case message of
            WS.Binary bytes ->
              case decodeFrame bytes of
                Right frame -> dispatch frame
                Left problem -> debug_ $ "live --serve: bad frame from the relay: " <> problem
            WS.Text bytes _ ->
              case parseServerMessage bytes of
                Notice message -> Progress.report $ D.fillSep ([ "───>", D.yellow "Live relay:" ] ++ fmap (D.fromChars . T.unpack) (T.words message))
                Refused message -> throwIO (Lost (T.unpack message))
                _ -> pure ()

      wTid <- forkIO writer
      pTid <- forkIO watchdog
      result <- try receiveLoop
      killThread wTid
      killThread pTid
      ids <- Map.keys <$> readTVarIO streams
      mapM_ closeStream ids
      case result of
        Right () -> pure SessionEstablishedThenLost
        Left (e :: SomeException) ->
          case fromException e of
            Just ThreadKilled -> throwIO e
            _ -> do
              debug_ $ "live --serve: tunnel lost: " <> show e
              pure SessionEstablishedThenLost


{-| A stream's thread: it ends when its socket is closed under it (the stream reset, the
tunnel lost) or it is killed, which is not worth printing. -}
forkQuiet :: IO () -> IO ThreadId
forkQuiet io =
  forkIO $ io `catch` \(e :: SomeException) ->
    case fromException e of
      Just ThreadKilled -> pure ()
      _ -> debug_ $ "live --serve: stream ended: " <> show e


{-| A TCP connection to `lamdera live`, or Nothing if it is not answering. -}
connectLocal :: Target -> IO (Maybe NS.Socket)
connectLocal (Target host port) = do
  result <- try $ do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    addr : _ <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
    sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
    (NS.connect sock (NS.addrAddress addr) >> pure sock)
      `catch` (\(e :: SomeException) -> NS.close sock >> throwIO e)
  case result of
    Right sock -> pure (Just sock)
    Left (e :: SomeException) -> do
      debug_ $ "live --serve: could not reach lamdera live at " <> host <> ":" <> show port <> ": " <> show e
      pure Nothing
