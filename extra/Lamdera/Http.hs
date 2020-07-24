{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Http where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Data.ByteString.Builder as BS
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import Reporting.Exit
import Reporting.Exit.Http
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Reporting.Task.Http as Http
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Task

required :: Text -> D.Decoder e a -> D.Decoder e (a -> b) -> D.Decoder e b
required key valDecoder decoder =
    custom (D.field key valDecoder) decoder


custom :: D.Decoder e a -> D.Decoder e (a -> b) -> D.Decoder e b
custom d1 d2 =
    D.map2 (\a_ fn_ -> fn_ a_) d1 d2


jsonHeaders =
  [ ( Http.hUserAgent, "lamdera-cli" )
  , ( Http.hContentType, "application/json" )
  , ( Http.hAccept, "application/json" )
  ]


httpPostJson manager request body =
  Client.httpLbs
    (request
      { Client.requestHeaders = jsonHeaders
      , Client.method = "POST"
      , Client.requestBody = Client.RequestBodyLBS $ BS.toLazyByteString $ E.encode body
      })
    manager


normalRpcJson body endpoint decoder =
  Http.run $ Http.anything endpoint $ \request manager ->
    do  debug $ "HTTP POST " <> endpoint <> "\n---> " <> (
          body
            & E.encodeUgly
            & BS.toLazyByteString
            & LBS.toStrict
            & T.decodeUtf8
            & T.unpack
          )
        response <- httpPostJson manager request body
        debug $ "<--- " <> (T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Client.responseBody response)

        let bytes = LBS.toStrict (Client.responseBody response)
        case D.parse endpoint id decoder bytes of
          Right value ->
            return $ Right value

          Left jsonProblem ->
            return $ Left $ BadJson endpoint jsonProblem


tryNormalRpcJson body endpoint decoder =
  Lamdera.Task.tryEither Progress.silentReporter $ normalRpcJson body endpoint decoder


-- Custom handler to extract message text from HTTP failures
-- The error handling in Elm is pretty deeply nested to throw away things
-- see `handleAnyError` function and follow it back.
-- This seemed the most pragmatic workaround.
-- Note: also tried to color the error red using Reporting.Doc, however
-- didn't work out easily in this context as the toString renderer drops colors,
-- seems the ANSI rendering needs to write straight out to terminal with IO ()
errorToString err =
  case err of
    BadHttp str (Unknown err) ->
      -- show err
      if textContains "\\\"error\\\":" (T.pack err)
        then
          -- This looks like a LamderaRPC error, rudimentary split out the error
          err
            & T.pack
            & T.splitOn "\\\""
            & reverse
            & flip (!!) 1
            & (<>) "error: "
            & T.unpack
        else
          -- Get the message part after the header preamble
          err
            & T.pack
            & T.splitOn "HTTP/1.1\n}\n"
            & last
            & (<>) "error:"
            & T.unpack

    _ ->
      -- Normal flow for everything else
      Reporting.Exit.toString err
