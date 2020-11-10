{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lamdera.Http where

{- HTTP helpers and wrapper
-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Data.ByteString.Builder as BS
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import Reporting.Exit
import qualified Http
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Stuff as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Task
import StandaloneInstances


data WithErrorField a
  = SuccessField a
  | ErrorField Text


-- @LAMDERA temporary there is already a setup for user agent + headers in Http.hs,
-- see if we can reconcile to that instead, as it includes the package
jsonHeaders =
  [ ( Http.hUserAgent, "lamdera-cli" )
  , ( Http.hContentType, "application/json" )
  , ( Http.hAccept, "application/json" )
  ]


-- @LAMDERA temporary remove
-- httpGetJson manager request =
--   Client.httpLbs
--     (request
--       { Client.requestHeaders = jsonHeaders
--       , Client.method = "GET"
--       })
--     manager


normalJson :: String -> String -> D.Decoder () a -> IO (Either Error a)
normalJson debugIdentifier url decoder = do
  manager <- Http.getManager
  debug $ "HTTP GET " <> url <> " (" <> debugIdentifier <> ")"
  Http.get manager url jsonHeaders HttpError $ \body ->
    case D.fromByteString decoder body of
      Right content ->
        return $ Right content

      Left problem ->
        return $ Left (JsonError url problem)


data Error
  = JsonError String (D.Error ())
  | HttpError Http.Error
  deriving (Show)


-- httpPostJson manager request body =
--   Client.httpLbs
--     (request
--       { Client.requestHeaders = jsonHeaders
--       , Client.method = "POST"
--       , Client.requestBody = Client.RequestBodyLBS $ BS.toLazyByteString $ E.encode body
--       })
--     manager


normalRpcJson :: String -> E.Value -> String -> D.Decoder () a -> IO (Either Error a)
normalRpcJson debugIdentifier body url decoder = do
  manager <- Http.getManager
  debug $ "POSTING   " <> url <> " (" <> debugIdentifier <> ", " <> show body <> ")"
  Http.postBody manager url jsonHeaders body HttpError $ \body ->
    case D.fromByteString decoder body of
      Right content ->
        return $ Right content

      Left problem ->
        return $ Left (JsonError url problem)


-- normalRpcJson debugIdentifier body endpoint decoder =
--   Http.run $ Http.anything endpoint $ \request manager ->
--     do  debug $ "HTTP POST " <> endpoint <> " (" <> debugIdentifier <> ")\n---> " <> (
--           body
--             & E.encodeUgly
--             & BS.toLazyByteString
--             & LBS.toStrict
--             & T.decodeUtf8
--             & T.unpack
--           )
--         response <- httpPostJson manager request body
--         debug $ "<--- " <> (T.unpack $ T.decodeUtf8 $ LBS.toStrict $ Client.responseBody response)
--
--         let bytes = LBS.toStrict (Client.responseBody response)
--         case D.parse endpoint id decoder bytes of
--           Right value ->
--             return $ Right value
--
--           Left jsonProblem ->
--             return $ Left $ BadJson endpoint jsonProblem


-- tryNormalRpcJson debugIdentifier body endpoint decoder =
--   Lamdera.Task.tryEither Progress.silentReporter $ normalRpcJson debugIdentifier body endpoint decoder


-- Custom handler to extract message text from HTTP failures
-- The error handling in Elm is pretty deeply nested to throw away things
-- see `handleAnyError` function and follow it back.
-- This seemed the most pragmatic workaround.
-- Note: also tried to color the error red using Reporting.Doc, however
-- didn't work out easily in this context as the toString renderer drops colors,
-- seems the ANSI rendering needs to write straight out to terminal with IO ()
errorToString err =
  -- @LAMDERA TEMPORARY
  show err
  --  Need to find the new way Http.Error

  -- case err of
  --   BadHttp str (Unknown err) ->
  --     -- show err
  --     if textContains "\\\"error\\\":" (T.pack err)
  --       then
  --         -- This looks like a LamderaRPC error, rudimentary split out the error
  --         err
  --           & T.pack
  --           & T.splitOn "\\\""
  --           & reverse
  --           & flip (!!) 1
  --           & (<>) "error: "
  --           & T.unpack
  --       else
  --         -- Get the message part after the header preamble
  --         err
  --           & T.pack
  --           & T.splitOn "HTTP/1.1\n}\n"
  --           & last
  --           & (<>) "error:"
  --           & T.unpack
  --
  --   _ ->
  --     -- Normal flow for everything else
  --     Reporting.Exit.toString err
