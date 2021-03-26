{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE CPP                   #-}
module Network.HTTP.ReverseProxy
    ( -- * Types
      ProxyDest (..)
      -- * Raw
    , rawProxyTo
    , rawTcpProxyTo
      -- * WAI + http-conduit
    , waiProxyTo
    , defaultOnExc
    , waiProxyToSettings
    , WaiProxyResponse (..)
      -- ** Settings
    , WaiProxySettings
    , defaultWaiProxySettings
    , wpsOnExc
    , wpsTimeout
    , wpsSetIpHeader
    , wpsProcessBody
    , wpsUpgradeToRaw
    , wpsGetDest
    , SetIpHeader (..)
      -- *** Local settings
    , LocalWaiProxySettings
    , defaultLocalWaiProxySettings
    , setLpsTimeBound
    {- FIXME
      -- * WAI to Raw
    , waiToRaw
    -}
    , addCors
    ) where

import           Blaze.ByteString.Builder       (Builder, fromByteString,
                                                 toLazyByteString)
import           Control.Applicative            ((<$>), (<|>))
import           Control.Monad                  (unless)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as S
import qualified Data.ByteString.Char8          as S8
import qualified Data.ByteString.Lazy           as L
import qualified Data.CaseInsensitive           as CI
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import qualified Data.Conduit.Network           as DCN
import           Data.Functor.Identity          (Identity (..))
import           Data.IORef
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    (mappend, mconcat, (<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Streaming.Network         (AppData, readLens)
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TLE
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.Word8                     (isSpace, _colon, _cr)
import           GHC.Generics                   (Generic)
import           Network.HTTP.Client            (BodyReader, brRead)
import qualified Network.HTTP.Client            as HC
import qualified Network.HTTP.Types             as HT
import qualified Network.Wai                    as WAI
import           Network.Wai.Logger             (showSockAddr)
import           UnliftIO                       (MonadIO, liftIO, MonadUnliftIO, timeout, SomeException, try, bracket, concurrently_)


import Lamdera
import qualified Data.List                      as List

-- | Host\/port combination to which we want to proxy.
data ProxyDest = ProxyDest
    { pdHost :: !ByteString
    , pdPort :: !Int
    } deriving (Read, Show, Eq, Ord, Generic)

-- | Set up a reverse proxy server, which will have a minimal overhead.
--
-- This function uses raw sockets, parsing as little of the request as
-- possible. The workflow is:
--
-- 1. Parse the first request headers.
--
-- 2. Ask the supplied function to specify how to reverse proxy.
--
-- 3. Open up a connection to the given host\/port.
--
-- 4. Pass all bytes across the wire unchanged.
--
-- If you need more control, such as modifying the request or response, use 'waiProxyTo'.
rawProxyTo :: MonadUnliftIO m
           => (HT.RequestHeaders -> m (Either (DCN.AppData -> m ()) ProxyDest))
           -- ^ How to reverse proxy. A @Left@ result will run the given
           -- 'DCN.Application', whereas a @Right@ will reverse proxy to the
           -- given host\/port.
           -> AppData -> m ()
rawProxyTo getDest appdata = do
    (rsrc, headers) <- liftIO $ fromClient $$+ getHeaders
    edest <- getDest headers
    case edest of
        Left app -> do
            -- We know that the socket will be closed by the toClient side, so
            -- we can throw away the finalizer here.
            irsrc <- liftIO $ newIORef rsrc
            let readData = do
                    rsrc1 <- readIORef irsrc
                    (rsrc2, mbs) <- rsrc1 $$++ await
                    writeIORef irsrc rsrc2
                    return $ fromMaybe "" mbs
            app $ runIdentity (readLens (const (Identity readData)) appdata)


        Right (ProxyDest host port) -> liftIO $ DCN.runTCPClient (DCN.clientSettings port host) (withServer rsrc)
  where
    fromClient = DCN.appSource appdata
    toClient = DCN.appSink appdata
    withServer rsrc appdataServer = concurrently_
        (rsrc $$+- toServer)
        (runConduit $ fromServer .| toClient)
      where
        fromServer = DCN.appSource appdataServer
        toServer = DCN.appSink appdataServer

-- | Set up a reverse tcp proxy server, which will have a minimal overhead.
--
-- This function uses raw sockets, parsing as little of the request as
-- possible. The workflow is:
--
-- 1. Open up a connection to the given host\/port.
--
-- 2. Pass all bytes across the wire unchanged.
--
-- If you need more control, such as modifying the request or response, use 'waiProxyTo'.
--
-- Since 0.4.4
rawTcpProxyTo :: MonadIO m
           => ProxyDest
           -> AppData
           -> m ()
rawTcpProxyTo (ProxyDest host port) appdata = liftIO $
    DCN.runTCPClient (DCN.clientSettings port host) withServer
  where
    withServer appdataServer = concurrently_
      (runConduit $ DCN.appSource appdata       .| DCN.appSink appdataServer)
      (runConduit $ DCN.appSource appdataServer .| DCN.appSink appdata      )

-- | Sends a simple 502 bad gateway error message with the contents of the
-- exception.
defaultOnExc :: SomeException -> WAI.Application
defaultOnExc exc _ sendResponse = sendResponse $ WAI.responseLBS
    HT.status502
    [("content-type", "text/plain")]
    ("Error connecting to gateway:\n\n" <> TLE.encodeUtf8 (TL.pack $ show exc))

-- | The different responses that could be generated by a @waiProxyTo@ lookup
-- function.
--
-- Since 0.2.0
data WaiProxyResponse = WPRResponse WAI.Response
                        -- ^ Respond with the given WAI Response.
                        --
                        -- Since 0.2.0
                      | WPRProxyDest ProxyDest
                        -- ^ Send to the given destination.
                        --
                        -- Since 0.2.0
                      | WPRProxyDestSecure ProxyDest
                        -- ^ Send to the given destination via HTTPS.
                      | WPRModifiedRequest WAI.Request ProxyDest
                        -- ^ Send to the given destination, but use the given
                        -- modified Request for computing the reverse-proxied
                        -- request. This can be useful for reverse proxying to
                        -- a different path than the one specified. By the
                        -- user.
                        -- The path will be taken from rawPathInfo while
                        -- the queryString from rawQueryString of the
                        -- request.
                        --
                        -- Since 0.2.0
                      | WPRModifiedRequestSecure WAI.Request ProxyDest
                        -- ^ Same as WPRModifiedRequest but send to the
                        -- given destination via HTTPS.
                      | WPRApplication WAI.Application
                        -- ^ Respond with the given WAI Application.
                        --
                        -- Since 0.4.0


instance Show WaiProxyResponse where
  show x =
    case x of
      WPRResponse response -> "WPRResponse " ++ show (WAI.responseStatus response, WAI.responseHeaders response)
      WPRProxyDest proxyDest -> "WPRProxyDest " ++ show proxyDest
      WPRProxyDestSecure proxyDest -> "WPRProxyDestSecure " ++ show proxyDest
      WPRModifiedRequest request proxyDest -> "WPRModifiedRequest " ++ show request ++ ", " ++ show proxyDest
      WPRModifiedRequestSecure request proxyDest -> "WPRModifiedRequestSecure " ++ show request ++ ", " ++ show proxyDest
      WPRApplication application -> "WPRApplication WAI.Application"



-- | Creates a WAI 'WAI.Application' which will handle reverse proxies.
--
-- Connections to the proxied server will be provided via http-conduit. As
-- such, all requests and responses will be fully processed in your reverse
-- proxy. This allows you much more control over the data sent over the wire,
-- but also incurs overhead. For a lower-overhead approach, consider
-- 'rawProxyTo'.
--
-- Most likely, the given application should be run with Warp, though in theory
-- other WAI handlers will work as well.
--
-- Note: This function will use chunked request bodies for communicating with
-- the proxied server. Not all servers necessarily support chunked request
-- bodies, so please confirm that yours does (Warp, Snap, and Happstack, for example, do).
waiProxyTo :: (WAI.Request -> IO WaiProxyResponse)
           -- ^ How to reverse proxy.
           -> (SomeException -> WAI.Application)
           -- ^ How to handle exceptions when calling remote server. For a
           -- simple 502 error page, use 'defaultOnExc'.
           -> HC.Manager -- ^ connection manager to utilize
           -> WAI.Application
waiProxyTo getDest onError = waiProxyToSettings getDest defaultWaiProxySettings { wpsOnExc = onError }

data LocalWaiProxySettings = LocalWaiProxySettings
    { lpsTimeBound :: Maybe Int
    -- ^ Allows to specify the maximum time allowed for the conection on per request basis.
    --
    -- Default: no timeouts
    --
    -- Since 0.4.2
    }

-- | Default value for 'LocalWaiProxySettings', same as 'def' but with a more explicit name.
--
-- Since 0.4.2
defaultLocalWaiProxySettings :: LocalWaiProxySettings
defaultLocalWaiProxySettings = LocalWaiProxySettings Nothing

-- | Allows to specify the maximum time allowed for the conection on per request basis.
--
-- Default: no timeouts
--
-- Since 0.4.2
setLpsTimeBound :: Maybe Int -> LocalWaiProxySettings -> LocalWaiProxySettings
setLpsTimeBound x s = s { lpsTimeBound = x }

data WaiProxySettings = WaiProxySettings
    { wpsOnExc :: SomeException -> WAI.Application
    , wpsTimeout :: Maybe Int
    , wpsSetIpHeader :: SetIpHeader
    -- ^ Set the X-Real-IP request header with the client's IP address.
    --
    -- Default: SIHFromSocket
    --
    -- Since 0.2.0
    , wpsProcessBody :: WAI.Request -> HC.Response () -> Maybe (ConduitT ByteString (Flush Builder) IO ())
    -- ^ Post-process the response body returned from the host.
    --   The API for this function changed to include the extra 'WAI.Request'
    --   parameter in version 0.5.0.
    --
    -- Since 0.2.1
    , wpsUpgradeToRaw :: WAI.Request -> Bool
    -- ^ Determine if the request should be upgraded to a raw proxy connection,
    -- as is needed for WebSockets. Requires WAI 2.1 or higher and a WAI
    -- handler with raw response support (e.g., Warp) to work.
    --
    -- Default: check if the upgrade header is websocket.
    --
    -- Since 0.3.1
    , wpsGetDest :: Maybe (WAI.Request -> IO (LocalWaiProxySettings, WaiProxyResponse))
    -- ^ Allow to override proxy settings for each request.
    -- If you supply this field it will take precedence over
    -- getDest parameter in waiProxyToSettings
    --
    -- Default: have one global setting
    --
    -- Since 0.4.2
    }

-- | How to set the X-Real-IP request header.
--
-- Since 0.2.0
data SetIpHeader = SIHNone -- ^ Do not set the header
                 | SIHFromSocket -- ^ Set it from the socket's address.
                 | SIHFromHeader -- ^ Set it from either X-Real-IP or X-Forwarded-For, if present

-- | Default value for 'WaiProxySettings'
--
-- @since 0.6.0
defaultWaiProxySettings :: WaiProxySettings
defaultWaiProxySettings = WaiProxySettings
        { wpsOnExc = defaultOnExc
        , wpsTimeout = Nothing
        , wpsSetIpHeader = SIHFromSocket
        , wpsProcessBody = \_ _ -> Nothing
        , wpsUpgradeToRaw = \req ->
            (CI.mk <$> lookup "upgrade" (WAI.requestHeaders req)) == Just "websocket"
        , wpsGetDest = Nothing
        }

renderHeaders :: WAI.Request -> HT.RequestHeaders -> Builder
renderHeaders req headers
    = fromByteString (WAI.requestMethod req)
   <> fromByteString " "
   <> fromByteString (WAI.rawPathInfo req)
   <> fromByteString (WAI.rawQueryString req)
   <> (if WAI.httpVersion req == HT.http11
           then fromByteString " HTTP/1.1"
           else fromByteString " HTTP/1.0")
   <> mconcat (map goHeader headers)
   <> fromByteString "\r\n\r\n"
  where
    goHeader (x, y)
        = fromByteString "\r\n"
       <> fromByteString (CI.original x)
       <> fromByteString ": "
       <> fromByteString y

tryWebSockets :: WaiProxySettings -> ByteString -> Int -> WAI.Request -> (WAI.Response -> IO b) -> IO b -> IO b
tryWebSockets wps host port req sendResponse fallback
    | wpsUpgradeToRaw wps req =
        sendResponse $ flip WAI.responseRaw backup $ \fromClientBody toClient ->
            DCN.runTCPClient settings $ \server ->
                let toServer = DCN.appSink server
                    fromServer = DCN.appSource server
                    fromClient = do
                        mapM_ yield $ L.toChunks $ toLazyByteString headers
                        let loop = do
                                bs <- liftIO fromClientBody
                                unless (S.null bs) $ do
                                    yield bs
                                    loop
                        loop
                    toClient' = awaitForever $ liftIO . toClient
                    headers = renderHeaders req $ fixReqHeaders wps req
                 in concurrently_
                        (runConduit $ fromClient .| toServer)
                        (runConduit $ fromServer .| toClient')
    | otherwise = fallback
  where
    backup = WAI.responseLBS HT.status500 [("Content-Type", "text/plain")]
        "http-reverse-proxy detected WebSockets request, but server does not support responseRaw"
    settings = DCN.clientSettings port host

strippedHeaders :: Set HT.HeaderName
strippedHeaders = Set.fromList
    ["content-length", "transfer-encoding", "accept-encoding", "content-encoding"]

fixReqHeaders :: WaiProxySettings -> WAI.Request -> HT.RequestHeaders
fixReqHeaders wps req =
    addXRealIP $ filter (\(key, value) -> not $ key `Set.member` strippedHeaders
                                       || (key == "connection" && value == "close"))
               $ WAI.requestHeaders req
  where
    fromSocket = (("X-Real-IP", S8.pack $ showSockAddr $ WAI.remoteHost req):)
    fromForwardedFor = do
      h <- lookup "x-forwarded-for" (WAI.requestHeaders req)
      listToMaybe $ map (TE.encodeUtf8 . T.strip) $ T.splitOn "," $ TE.decodeUtf8 h
    addXRealIP =
        case wpsSetIpHeader wps of
            SIHFromSocket -> fromSocket
            SIHFromHeader ->
                case lookup "x-real-ip" (WAI.requestHeaders req) <|> fromForwardedFor of
                    Nothing -> fromSocket
                    Just ip -> (("X-Real-IP", ip):)
            SIHNone -> id

waiProxyToSettings :: (WAI.Request -> IO WaiProxyResponse)
                   -> WaiProxySettings
                   -> HC.Manager
                   -> WAI.Application
waiProxyToSettings getDest wps' manager req0 sendResponse = do
    let wps = wps'{wpsGetDest = wpsGetDest wps' <|> Just (fmap (LocalWaiProxySettings $ wpsTimeout wps',) . getDest)}
    (lps, edest') <- fromMaybe
        (const $ return (defaultLocalWaiProxySettings, WPRResponse $ WAI.responseLBS HT.status500 [] "proxy not setup"))
        (wpsGetDest wps)
        req0
    let edest =
            case edest' of
                WPRResponse res -> Left $ \_req -> ($ res)
                WPRProxyDest pd -> Right (pd, req0, False)
                WPRProxyDestSecure pd -> Right (pd, req0, True)
                WPRModifiedRequest req pd -> Right (pd, req, False)
                WPRModifiedRequestSecure req pd -> Right (pd, req, True)
                WPRApplication app -> Left app
        timeBound us f =
            timeout us f >>= \case
                Just res -> return res
                Nothing -> sendResponse $ WAI.responseLBS HT.status500 [] "timeBound"
    case edest of
        Left app -> maybe id timeBound (lpsTimeBound lps) $ app req0 sendResponse
        Right (ProxyDest host port, req, secure) -> tryWebSockets wps host port req sendResponse $ do
            scb <- semiCachedBody (WAI.requestBody req)
            let body =
                  case WAI.requestBodyLength req of
                      WAI.KnownLength i -> HC.RequestBodyStream (fromIntegral i) scb
                      WAI.ChunkedBody -> HC.RequestBodyStreamChunked scb

            let req' =
#if MIN_VERSION_http_client(0, 5, 0)
                  HC.defaultRequest
                    { HC.checkResponse = \_ _ -> return ()
                    , HC.responseTimeout = maybe HC.responseTimeoutNone HC.responseTimeoutMicro $ lpsTimeBound lps
#else
                  def
                    { HC.checkStatus = \_ _ _ -> Nothing
                    , HC.responseTimeout = lpsTimeBound lps
#endif
                    , HC.method = WAI.requestMethod req
                    , HC.secure = secure
                    , HC.host = host
                    , HC.port = port
                    , HC.path = WAI.rawPathInfo req
                    , HC.queryString = WAI.rawQueryString req
                    , HC.requestHeaders = fixReqHeaders wps req
                    , HC.requestBody = body
                    , HC.redirectCount = 0
                    }
            bracket
                (try $ HC.responseOpen req' manager)
                (either (const $ return ()) HC.responseClose)
                $ \case
                    Left e -> wpsOnExc wps e req sendResponse
                    Right res -> do
                        let conduit = fromMaybe
                                        (awaitForever (\bs -> yield (Chunk $ fromByteString bs) >> yield Flush))
                                        (wpsProcessBody wps req $ const () <$> res)
                            src = bodyReaderSource $ HC.responseBody res

                            responseStatus = (HC.responseStatus res)
                            responseHeaders =
                              (HC.responseHeaders res
                                & filter (\(key, _) -> not $ key `Set.member` strippedHeaders)
                                & addCors req
                              )
                        -- debug_ $ "⬅️  " ++ show (responseStatus, responseHeaders)
                        sendResponse $ WAI.responseStream
                            responseStatus
                            responseHeaders
                            (\sendChunk flush -> runConduit $ src .| conduit .| CL.mapM_ (\mb ->
                                case mb of
                                    Flush -> flush
                                    Chunk b -> sendChunk b))


-- | Introduce a minor level of caching to handle some basic
-- retry cases inside http-client. But to avoid a DoS attack,
-- don't cache more than 65535 bytes (the theoretical max TCP packet size).
--
-- See: <https://github.com/fpco/http-reverse-proxy/issues/34#issuecomment-719136064>
semiCachedBody :: IO ByteString -> IO (HC.GivesPopper ())
semiCachedBody orig = do
  ref <- newIORef $ SCBCaching 0 []
  pure $ \needsPopper -> do
    let fromChunks len chunks =
          case NE.nonEmpty (reverse chunks) of
            Nothing -> SCBCaching len chunks
            Just toDrain -> SCBDraining len chunks toDrain
    state0 <- readIORef ref >>=
      \case
        SCBCaching len chunks -> pure $ fromChunks len chunks
        SCBDraining len chunks _ -> pure $ fromChunks len chunks
        SCBTooMuchData -> error "Cannot retry this request body, need to force a new request"
    writeIORef ref $! state0
    let popper :: IO ByteString
        popper = do
          readIORef ref >>=
            \case
              SCBDraining len chunks (next:|rest) -> do
                writeIORef ref $!
                  case rest of
                    [] -> SCBCaching len chunks
                    x:xs -> SCBDraining len chunks (x:|xs)
                pure next
              SCBTooMuchData -> orig
              SCBCaching len chunks -> do
                bs <- orig
                let newLen = len + S.length bs
                writeIORef ref $!
                  if newLen > maxCache
                    then SCBTooMuchData
                    else SCBCaching newLen (bs:chunks)
                pure bs

    needsPopper popper
  where
    maxCache = 65535

data SCB
  = SCBCaching !Int ![ByteString]
  | SCBDraining !Int ![ByteString] !(NonEmpty ByteString)
  | SCBTooMuchData

-- | Get the HTTP headers for the first request on the stream, returning on
-- consumed bytes as leftovers. Has built-in limits on how many bytes it will
-- consume (specifically, will not ask for another chunked after it receives
-- 1000 bytes).
getHeaders :: Monad m => ConduitT ByteString o m HT.RequestHeaders
getHeaders =
    toHeaders <$> go id
  where
    go front =
        await >>= maybe close push
      where
        close = leftover bs >> return bs
          where
            bs = front S8.empty
        push bs'
            | "\r\n\r\n" `S8.isInfixOf` bs
              || "\n\n" `S8.isInfixOf` bs
              || S8.length bs > 4096 = leftover bs >> return bs
            | otherwise = go $ mappend bs
          where
            bs = front bs'
    toHeaders = map toHeader . takeWhile (not . S8.null) . drop 1 . S8.lines
    toHeader bs =
        (CI.mk key, val)
      where
        (key, bs') = S.break (== _colon) bs
        val = S.takeWhile (/= _cr) $ S.dropWhile isSpace $ S.drop 1 bs'

{- FIXME
-- | Convert a WAI application into a raw application, using Warp.
waiToRaw :: WAI.Application -> DCN.Application IO
waiToRaw app appdata0 =
    loop fromClient0
  where
    fromClient0 = DCN.appSource appdata0
    toClient = DCN.appSink appdata0
    loop fromClient = do
        mfromClient <- runResourceT $ withInternalState $ \internalState -> do
            ex <- try $ parseRequest conn internalState dummyAddr fromClient
            case ex of
                Left (_ :: SomeException) -> return Nothing
                Right (req, fromClient') -> do
                    res <- app req
                    keepAlive <- sendResponse
                        defaultSettings
                        req conn res
                    (fromClient'', _) <- liftIO fromClient' >>= unwrapResumable
                    return $ if keepAlive then Just fromClient'' else Nothing
        maybe (return ()) loop mfromClient

    dummyAddr = SockAddrInet (PortNum 0) 0 -- FIXME
    conn = Connection
        { connSendMany = \bss -> mapM_ yield bss $$ toClient
        , connSendAll = \bs -> yield bs $$ toClient
        , connSendFile = \fp offset len _th headers _cleaner ->
            let src1 = mapM_ yield headers
                src2 = sourceFileRange fp (Just offset) (Just len)
             in runResourceT
                $  (src1 >> src2)
                $$ transPipe lift toClient
        , connClose = return ()
        , connRecv = error "connRecv should not be used"
        }
        -}

bodyReaderSource :: MonadIO m => BodyReader -> ConduitT i ByteString m ()
bodyReaderSource br =
    loop
  where
    loop = do
        bs <- liftIO $ brRead br
        unless (S.null bs) $ do
            yield bs
            loop






------------------------------------------------------------------ Modifications

-- Based on https://github.com/Rob--W/cors-anywhere/blob/master/lib/cors-anywhere.js#L53
addCors req responseHeaders =
  let headers = WAI.requestHeaders req
  in
  responseHeaders
    & listUpsert (hasHeader_ "Access-Control-Allow-Origin") ("Access-Control-Allow-Origin", "*")
    & addIf (hasHeader "Access-Control-Request-Method" headers)
        "Access-Control-Allow-Methods" (header_ "Access-Control-Request-Method" headers)
    & addIf (hasHeader "Access-Control-Request-Headers" headers)
        "Access-Control-Allow-Headers" (header_ "Access-Control-Request-Headers" headers)

  -- @TODO future, needed?
  -- & addIf (WAI.requestMethod req == "OPTIONS")
  --     "access-control-max-age" "0"


add k v l =
  (k,v):l

addIf bool k v l =
  if bool then
    (k,v):l
  else
    l

header_ name headers =
  case List.find (\(k,v) -> k == name) headers of
    Just (_,v) -> v
    Nothing -> error $ "header_ not found: " <> show name

hasHeader name headers =
  case List.find (\(k,v) -> k == name) headers of
    Just _ -> True
    Nothing -> False

hasHeader_ name (k,v) =
  k == name
