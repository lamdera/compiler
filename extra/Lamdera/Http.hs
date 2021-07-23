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
import qualified Network.HTTP.Client as HTTP

import Lamdera
import Lamdera.Progress
import StandaloneInstances


data WithErrorField a
  = SuccessField a
  | ErrorField Text
  deriving (Show)


jsonHeaders :: [Http.Header]
jsonHeaders =
  [ ( Http.hUserAgent, "lamdera-" <> BS.pack lamderaVersionString )
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


printHttpError :: Error -> String -> IO ()
printHttpError error reason =
  case error of
    JsonError string dError -> putStrLn $ show error

    HttpError httpError ->
      throw $ toHttpErrorReport "HTTP PROBLEM" httpError reason


-- Going based off the error outlines in `toHttpErrorReport`,
-- this function helps us avoid certain actions if it looks like the
-- HTTP request failed because of network errors
isOfflineError :: Error -> Bool
isOfflineError error =
  case error of
    JsonError string dError -> False

    HttpError httpError ->
      case httpError of
        Http.BadUrl url reason ->
          False

        Http.BadHttp url httpExceptionContent ->
          case httpExceptionContent of
            HTTP.StatusCodeException response body ->
              False

            HTTP.TooManyRedirects responses ->
              False

            otherException ->
              True

        Http.BadMystery url someException ->
          True
