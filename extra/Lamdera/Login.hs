{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Login where

import Prelude hiding (init)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad
import Data.Maybe (fromMaybe)

-- HTTP
import qualified Data.Text as T
import qualified Data.ByteString.Builder as BS
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Client as Client
import qualified Reporting.Task as Task
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Reporting.Exit
import Lamdera.Http

import qualified Elm.PerUserCache as PerUserCache
import System.FilePath ((</>))
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Project


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting login..."

  inProduction <- Lamdera.Project.inProduction
  appName <- Lamdera.Project.maybeAppName

  token <- do
    elmHome <- PerUserCache.getElmHome
    existingToken <- readUtf8Text (elmHome </> ".lamdera-cli")
    newToken <- UUID.toText <$> UUID.nextRandom
    case existingToken of
      Just token -> do

        apiSession <- fetchApiSession (appName & fromMaybe "test-local") token True

        case apiSession of
          Right "success" -> do
            pure token

          _ -> do
            pDocLn $ D.fillSep ["───>", D.red "Existing token invalid, starting again"]
            remove (elmHome </> ".lamdera-cli")
            pure newToken

      Nothing ->
        pure newToken

  doUntil ((==) True) $
    checkApiLoop inProduction (appName & fromMaybe "test-local") token

  pure ()


validateCliToken :: IO Text
validateCliToken = do
  appName <- Lamdera.Project.maybeAppName
  elmHome <- PerUserCache.getElmHome
  existingToken <- readUtf8Text (elmHome </> ".lamdera-cli")

  case existingToken of
    Just token -> do

      apiSession <- fetchApiSession (appName & fromMaybe "test-local") token True

      case apiSession of
        Right "success" -> do
          pure token

        _ -> do
          pDocLn $ D.fillSep ["───>", D.red "Existing token invalid, please run `lamdera login`"]
          remove (elmHome </> ".lamdera-cli")
          fail "Invalid token"

    Nothing -> do
      pDocLn $ D.fillSep ["───>", D.red "No CLI auth, please run `lamdera login`"]
      fail "Invalid token"




doUntil :: (a -> Bool) -> (IO a) -> IO a
doUntil check fn = do
  x <- fn
  if check x
    then pure x
    else doUntil check fn


checkApiLoop inProduction appName token = do
  apiSession <- fetchApiSession appName token True
  case apiSession of
    Right response ->
      case response of
        "init" -> do

          let
            url =
              if inProduction
                then "https://dashboard.lamdera.app/auth/cli/" <> token
                else "http://localhost:8000/auth/cli/" <> token

          putStrLn $ "───> Opening " <> T.unpack url
          openUrlInBrowser url
          putStrLn $ "───> Waiting for authorization..."
          sleep 1000
          pure False

        "pending" -> do
          sleep 1000
          pure False

        "invalid" -> do
          pDocLn $ D.fillSep ["───>", D.red "Error:", D.reflow "invalid token. Please try again."]
          pure True

        "success" -> do
          elmHome <- PerUserCache.getElmHome
          writeUtf8 (elmHome </> ".lamdera-cli") token
          -- putStrLn $ "───> Logged in!"
          pDocLn $ D.fillSep ["───>", D.green "Logged in!"]
          pure True

    Left err -> do
      putStrLn $ Lamdera.Http.errorToString err
      pure True


fetchApiSession :: Text -> Text -> Bool -> IO (Either Reporting.Exit.Exit Text)
fetchApiSession appName token useLocal =
  let
    endpoint =
      if textContains "-local" appName && useLocal
        then
          "http://localhost:8000/_r/apiSessionJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://" <> T.unpack appName <> ".lamdera.app/_r/apiSessionJson"

    body =
      E.object [ ("token", E.text token) ]

    decoder =
      D.text

  in
  Lamdera.Http.tryNormalRpcJson "fetchApiSession" body endpoint decoder
