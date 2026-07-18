{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Http where

{- HTTP helpers and wrapper
-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Http
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified System.Directory as Dir

import qualified Http
import qualified Json.Decode as D
import qualified Json.Encode as E
import Reporting.Exit

import Lamdera
import Lamdera.Progress
import qualified Lamdera.Version
import StandaloneInstances


data WithErrorField a
  = SuccessField a
  | ErrorField Text
  deriving (Show)


defaultHeaders :: [Http.Header]
defaultHeaders =
  [ ( Http.hUserAgent, "lamdera-" <> BS.pack Lamdera.Version.short )
  , ( Http.hAcceptEncoding, "gzip")
  ]


jsonHeaders :: [Http.Header]
jsonHeaders =
  defaultHeaders ++
    [ ( Http.hContentType, "application/json" )
    , ( Http.hAccept, "application/json" )
    ]


{-| The directory holding unix sockets: /run/lamdera-sockets if present, else
~/lamdera-sockets. -}
socketDir :: IO FilePath
socketDir = do
  onServer <- Dir.doesDirectoryExist "/run/lamdera-sockets"
  if onServer
    then pure "/run/lamdera-sockets"
    else do
      home <- Dir.getHomeDirectory
      pure (home <> "/lamdera-sockets")


{-| The unix socket path for a given name, if one exists locally. -}
socketPathIfExists :: Text -> IO (Maybe FilePath)
socketPathIfExists name = do
  dir <- socketDir
  let path = dir <> "/" <> T.unpack name <> ".sock"
  -- Sockets aren't regular files, so doesPathExist (not doesFileExist).
  exists <- Dir.doesPathExist path
  pure $ if exists then Just path else Nothing


{-| An HTTP Manager that connects to a unix socket instead of a TCP host/port.
The request URL's host is still used for the Host header; the raw connection
override ignores it and dials the socket. -}
socketManager :: FilePath -> IO HTTP.Manager
socketManager path =
  HTTP.newManager HTTP.defaultManagerSettings
    { HTTP.managerRawConnection = pure $ \_ _ _ -> do
        sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.connect sock (NS.SockAddrUnix path)
        HTTP.makeConnection
          (NSB.recv sock 8192)
          (NSB.sendAll sock)
          (NS.close sock)
    }


managerFor :: Maybe FilePath -> IO HTTP.Manager
managerFor Nothing = Http.getManager
managerFor (Just path) = socketManager path


normalJson :: (Show a) => String -> String -> D.Decoder () a -> IO (Either Error a)
normalJson = normalJsonVia Nothing


{-| Like normalJson, but routed over a unix socket when a path is given. -}
normalJsonVia :: (Show a) => Maybe FilePath -> String -> String -> D.Decoder () a -> IO (Either Error a)
normalJsonVia msocket debugIdentifier url decoder = do
  manager <- managerFor msocket
  debug $ "HTTP GET " <> url <> " (" <> debugIdentifier <> ")" <> maybe "" (\s -> " via socket " <> s) msocket
  Http.get manager url jsonHeaders HttpError $ \body ->
    case D.fromByteString decoder body of
      Right content ->
        -- Helpful for debugging
        -- return $ Right $ debugNote "response" content
        return $ Right content

      Left problem ->
        return $ Left (JsonError url problem)


data Error
  = JsonError String (D.Error ())
  | HttpError Http.Error
  deriving (Show)


normalRpcJson :: String -> E.Value -> String -> D.Decoder () a -> IO (Either Error a)
normalRpcJson = normalRpcJsonVia Nothing


{-| Like normalRpcJson, but routed over a unix socket when a path is given. -}
normalRpcJsonVia :: Maybe FilePath -> String -> E.Value -> String -> D.Decoder () a -> IO (Either Error a)
normalRpcJsonVia msocket debugIdentifier body url decoder = do
  manager <- managerFor msocket
  debug $ "POSTING   " <> url <> " (" <> debugIdentifier <> ", " <> show body <> ")" <> maybe "" (\s -> " via socket " <> s) msocket
  Http.postBody manager url jsonHeaders body HttpError $ \body ->
    case D.fromByteString decoder body of
      Right content ->
        return $ Right content

      Left problem ->
        return $ Left (JsonError url problem)


downloadToFile :: String -> FilePath -> IO (Either Error ())
downloadToFile url path = do
  manager <- Http.getManager
  Http.get manager url [] HttpError $ \body -> do
    BS.writeFile path body
    pure $ Right ()


printHttpError :: Error -> String -> IO ()
printHttpError error reason =
  case error of
    JsonError string dError -> atomicPutStrLn $ show error

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
