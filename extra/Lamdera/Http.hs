{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lamdera.Http where

{- HTTP helpers and wrapper
-}

import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Network.HTTP.Types.Header as Http
import Reporting.Exit
import qualified Http
import qualified Data.ByteString.Char8 as BS

import Lamdera
import StandaloneInstances


data WithErrorField a
  = SuccessField a
  | ErrorField Text
  deriving (Show)


jsonHeaders :: [Http.Header]
jsonHeaders =
  [ ( Http.hUserAgent, "lamdera-" <> BS.pack lamderaVersion )
  , ( Http.hContentType, "application/json" )
  , ( Http.hAccept, "application/json" )
  , ( Http.hAcceptEncoding, "gzip")
  ]


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
