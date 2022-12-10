{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.CLI.Login where

import Prelude hiding (init)

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import qualified System.IO as IO

-- HTTP
import qualified Data.Text as T
import qualified Data.ByteString.Builder as BS
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Client as Client
import qualified Reporting.Task as Task
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String
import qualified Reporting.Exit

import qualified Stuff as PerUserCache
import System.FilePath ((</>))
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Http
import qualified Lamdera.Project
import qualified Lamdera.Progress as Progress
import StandaloneInstances


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
            Progress.report $ D.fillSep ["───>", D.green "Logged in!"]

          Left err -> do
            if Lamdera.Http.isOfflineError err
              then do
                Lamdera.Http.printHttpError err "I needed to poll for the CLI session approval status"
                exitFailure
              else do
                Progress.report $ D.fillSep ["───>", D.red "Existing token invalid, starting again"]
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
            Progress.report $ D.fillSep ["───>", D.red "⚠️  Error:", D.reflow "invalid token. Please try again."]
            pure True

          "success" -> do
            elmHome <- PerUserCache.getElmHome
            writeUtf8 (elmHome </> ".lamdera-cli") token
            Progress.report $ D.fillSep ["───>", D.green "Logged in!"]
            pure True

          _ -> do
            -- Unexpected response... ignore and loop again
            sleep 1000
            pure False

      Left err -> do
        Lamdera.Http.printHttpError err "I needed to poll for the CLI session approval status"
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

        Left error -> do
          case error of
            Lamdera.Http.HttpError httpExit -> do
              Lamdera.Http.printHttpError error "I need to validate the CLI session"
              exitFailure

            _ -> do
              Progress.report $ D.fillSep ["───>", D.red "Invalid CLI auth, please re-run `lamdera login`"]
              remove (elmHome </> ".lamdera-cli")
              exitFailure

        _ -> do
          Progress.report $ D.fillSep ["───>", D.red "Invalid CLI auth, please re-run `lamdera login`"]
          remove (elmHome </> ".lamdera-cli")
          exitFailure


    Nothing -> do
      Progress.report $ D.fillSep ["───>", D.red "No CLI auth, please run `lamdera login`"]
      exitFailure



fetchApiSession :: Text -> Text -> IO (Either Lamdera.Http.Error Text)
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
      E.object [ ("token", E.string $ Json.String.fromChars $ T.unpack token) ]

    decoder =
      D.string

  in
  (fmap $ T.pack . Json.String.toChars) <$> Lamdera.Http.normalRpcJson "fetchApiSession" body endpoint decoder


doUntil :: (a -> Bool) -> (IO a) -> IO ()
doUntil check fn = do
  x <- fn
  if check x
    then pure ()
    else doUntil check fn
