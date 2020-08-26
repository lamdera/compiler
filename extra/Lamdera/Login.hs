{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Login where

import Prelude hiding (init)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)

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

  inProduction <- Lamdera.inProduction
  appName <- Lamdera.Project.appNameOrThrow

  token <- do
    elmHome <- PerUserCache.getElmHome
    existingToken <- readUtf8Text (elmHome </> ".lamdera-cli")
    newToken <- UUID.toText <$> UUID.nextRandom
    case existingToken of
      Just token -> do

        apiSession <- fetchApiSession appName token

        case apiSession of
          Right "success" -> do
            writeUtf8 (elmHome </> ".lamdera-cli") token
            pDocLn $ D.fillSep ["───>", D.green "Logged in!"]

          _ -> do
            pDocLn $ D.fillSep ["───>", D.red "Existing token invalid, starting again"]
            remove (elmHome </> ".lamdera-cli")
            checkApiLoop inProduction appName newToken

      Nothing -> do
        checkApiLoop inProduction appName newToken

  pure ()


checkApiLoop inProduction appName token =
  doUntil ((==) True) $ do
    apiSession <- fetchApiSession appName token
    case apiSession of
      Right response ->
        case response of
          "init" -> do
            let
              mask = textSha1 $ "x827c2" <> token
              url =
                if textContains "-local" appName
                  then
                    "http://localhost:8000/auth/cli/" <> mask
                  else
                    "https://dashboard.lamdera.app/auth/cli/" <> mask

            putStrLn $ "───> Opening " <> T.unpack url
            sleep 1000
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
            pDocLn $ D.fillSep ["───>", D.green "Logged in!"]
            pure True

          _ -> do
            -- Unexpected response... ignore and loop again
            sleep 1000
            pure False

      Left err -> do
        putStrLn $ Lamdera.Http.errorToString err
        pure True


validateCliToken :: IO Text
validateCliToken = do
  appName <- Lamdera.Project.appNameOrThrow
  elmHome <- PerUserCache.getElmHome
  existingToken <- readUtf8Text (elmHome </> ".lamdera-cli")

  case existingToken of
    Just token -> do

      apiSession <- fetchApiSession appName token

      case apiSession of
        Right "success" -> do
          pure token

        Left exit -> do
          case exit of
            Reporting.Exit.BadHttp string httpExit -> do
              pDocLn $ D.fillSep [ D.red "Oops, there appears to have been a HTTP error:\n"]
              putStrLn $ Lamdera.Http.errorToString exit
              pDocLn $ D.fillSep [ D.red "\nPlease double-check your internet connection, or report this issue.\n"]
              exitFailure

            _ -> do
              pDocLn $ D.fillSep ["───>", D.red "Invalid CLI auth, please re-run `lamdera login`"]
              remove (elmHome </> ".lamdera-cli")
              exitFailure

        _ -> do
          pDocLn $ D.fillSep ["───>", D.red "Invalid CLI auth, please re-run `lamdera login`"]
          remove (elmHome </> ".lamdera-cli")
          exitFailure


    Nothing -> do
      pDocLn $ D.fillSep ["───>", D.red "No CLI auth, please run `lamdera login`"]
      exitFailure



fetchApiSession :: Text -> Text -> IO (Either Reporting.Exit.Exit Text)
fetchApiSession appName token =
  let
    endpoint =
      if textContains "-local" appName
        then
          "http://localhost:8000/_r/apiSessionJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://dashboard.lamdera.app/_r/apiSessionJson"

    body =
      E.object [ ("token", E.text token) ]

    decoder =
      D.text

  in
  Lamdera.Http.tryNormalRpcJson "fetchApiSession" body endpoint decoder


doUntil :: (a -> Bool) -> (IO a) -> IO ()
doUntil check fn = do
  x <- fn
  if check x
    then pure ()
    else doUntil check fn
