{-# LANGUAGE OverloadedStrings #-}

module Lamdera.ReverseProxy where

import Control.Concurrent
import Control.Exception (throw)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import Text.Read (readMaybe)

import Network.HTTP.ReverseProxy
import Http
import Lamdera
import Ext.Common (trackGhciThread)

{- Rough implementation of a reverse proxy, mirroring some of the techniques from cors-anywhere:

https://github.com/Rob--W/cors-anywhere/blob/master/lib/cors-anywhere.js

Uses a vendored version of the http-reverse-proxy package (Network.HTTP.ReverseProxy)

extra/Network/HTTP/ReverseProxy.hs

-}


start :: IO ()
start = do
  debug_ "Starting proxy..."
  threadId <- startReverseProxy_
  trackGhciThread "Lamdera.ReverseProxy.start" threadId


startReverseProxy_ :: IO ThreadId
startReverseProxy_ = do
  forkIO $ do
    manager <- Http.getManager

    let
      settings =
        defaultSettings
          & setPort 8001
          -- Proxy should be for local access only
          & setHost "127.0.0.1"

    runSettings settings $
      waiProxyTo
        (\request ->
          case requestHeaderHost request of
            Just host -> do
              -- debug "------"
              -- debug_ $ show request

              res <-
                case requestMethod request of
                  "OPTIONS" ->
                    pure $ WPRResponse $ responseLBS status200 ([] & addCors request) ""

                  _ -> do
                    let
                      -- When a request comes in with a custom defined port, we need to extract that
                      -- port from the hostname and pass it to the proxy destination. Otherwise, the
                      -- proxy destination will use the default port (80 for http, 443 for https).
                      -- for example: https://example.com:3001 -> ProxyDestSecure "example.com" 3001
                      handleWithDefaultPort :: Text -> [Text] -> Int -> (Request -> ProxyDest -> WaiProxyResponse) -> IO WaiProxyResponse
                      handleWithDefaultPort hostname rest defaultPort wrapper = do
                        let
                          (hostnameClean, targetPort) =
                            case Text.splitOn ":" hostname of
                              hostnameClean:port:[] ->
                                (hostnameClean, port & Text.unpack & readMaybe & withDefault defaultPort)

                              _ ->
                                (hostname, defaultPort)

                        pure $ wrapper
                          (modReq rest hostnameClean request)
                          (ProxyDest (T.encodeUtf8 hostnameClean) targetPort)

                    case pathInfo request of
                      "http:":"":hostname:rest -> do
                        handleWithDefaultPort hostname rest 80 WPRModifiedRequest

                      "https:":"":hostname:rest -> do
                        handleWithDefaultPort hostname rest 443 WPRModifiedRequestSecure

                      path ->
                        pure $ WPRProxyFail $ "url missing http or https prefix: " <> Text.unpack (Text.intercalate "," path)

              -- debug_ $ "\n➡️  proxying with: " ++ show res
              pure res

            Nothing ->
              -- Impossible for our purposes, but possible with hand crafted curl shennanigans
              pure $ WPRProxyFail $ "request is missing host header: " <> show request
        )
        (\exception ->
          debug_note ("\n➡️❌  proxy error: " ++ show exception) defaultOnExc exception
        )
        manager


{- Adjust the request to remove the effects of the http://localhost:8001 URL prefix -}
modReq :: [Text] -> Text -> Network.Wai.Request -> Network.Wai.Request
modReq newPathInfo hostname request =
  request
    { pathInfo = newPathInfo
    , rawPathInfo = newPathInfo & Text.intercalate "/" & Text.append "/" & T.encodeUtf8
    , requestHeaderHost = Just $ T.encodeUtf8 hostname
    , requestHeaders =
        request
          & requestHeaders
          & fmap (\(header, contents) ->
            if header == hHost
              then (header, T.encodeUtf8 hostname)
              else (header, contents)
          )
    }
