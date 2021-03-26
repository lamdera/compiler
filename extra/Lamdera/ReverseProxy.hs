{-# LANGUAGE OverloadedStrings #-}

module Lamdera.ReverseProxy where

import Control.Exception (finally, throw)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Text.Read (readMaybe)

import Network.HTTP.ReverseProxy
import Control.Concurrent
import Control.Concurrent.MVar
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai
import Data.ByteString.Search
import Data.CaseInsensitive

import Http
import Lamdera
import Ext.Common (trackGhciThread)

{- Rough implementation of a reverse proxy, mirroring some of the techniques from cors-anywhere:

https://github.com/Rob--W/cors-anywhere/blob/master/lib/cors-anywhere.js

Uses a vendored version of the http-reverse-proxy package (Network.HTTP.ReverseProxy)

/Users/mario/dev/projects/lamdera-compiler/extra/Network/HTTP/ReverseProxy.hs

-}


start :: IO ()
start = do
  debug_ "Starting proxy..."
  threadId <- startReverseProxy_
  trackGhciThread threadId


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
                    case pathInfo request of
                      "http:":"":hostname:rest -> do
                        let
                          (hostnameClean, port) =
                            case Text.splitOn ":" hostname of
                              hostnameClean:port:[] ->
                                (hostnameClean, port & Text.unpack & readMaybe & withDefault 80)

                              _ ->
                                (hostname, 80)


                        pure $ WPRModifiedRequest
                          (modReq rest hostnameClean request)
                          (ProxyDest (T.encodeUtf8 hostnameClean) port)

                      "https:":"":hostname:rest ->
                        pure $ WPRModifiedRequestSecure
                          (modReq rest hostname request)
                          (ProxyDest (T.encodeUtf8 hostname) 443)

                      _ ->
                        error "bad path"

              -- debug_ $ "\n➡️  proxying with: " ++ show res
              pure res

            Nothing ->
              -- Impossible for our purposes
              -- debug_ "HTTP request without host... dropping"
              pure $ WPRProxyDestSecure (ProxyDest "lamdera.com" 80)
        )
        (\exception -> throw exception )
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
