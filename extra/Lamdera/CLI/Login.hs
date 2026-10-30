{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.CLI.Login where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String

import qualified Root
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Http
import qualified Lamdera.Project
import qualified Lamdera.Progress as Progress


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting login..."

  inProduction <- Lamdera.inProduction
  appName <- Lamdera.Project.appNameOrThrow

  token <- do
    elmHome <- Root.getElmHomePath
    existingToken <- getToken
    newToken <- UUID.toText <$> UUID.nextRandom
    case existingToken of
      Just token -> do
        apiSession <- fetchApiSession appName token

        case apiSession of
          Right "success" -> do
            writeToken token
            Progress.report $ D.fillSep ["───>", D.dullgreen "Logged in!"]

          Right _ -> do
            Progress.report $ D.fillSep ["───>", D.red "Unexpected response, starting again: ", D.fromChars $ show apiSession ]
            removeToken
            checkApiLoop inProduction appName newToken

          Left err -> do
            if Lamdera.Http.isOfflineError err
              then do
                Lamdera.Http.printHttpError err "I needed to poll for the CLI session approval status"
                exitFailure
              else do
                Progress.report $ D.fillSep ["───>", D.red "Existing token invalid, starting again"]
                removeToken
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
            web <- Lamdera.Http.dashboardWebUrl (Just appName)
            let
              mask = textSha1 $ "x827c2" <> token
              url =
                T.pack web <> "/auth/cli/" <> mask

            putStrLn $ "───> Opening " <> T.unpack url
            sleep 1000
            systemOpenPath url
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
            elmHome <- Root.getElmHomePath
            writeToken token
            Progress.report $ D.fillSep ["───>", D.dullgreen "Logged in!"]
            pure True

          _ -> do
            -- Unexpected response... ignore and loop again
            sleep 1000
            pure False

      Left err -> do
        Lamdera.Http.printHttpError err "I needed to poll for the CLI session approval status"
        pure True


tokenFile :: FilePath
tokenFile = ".lamdera-cli"


getToken :: IO (Maybe Text)
getToken = do
  elmHome <- Root.getElmHomePath
  tokenM <- readUtf8Text (elmHome </> tokenFile)
  pure $ fmap T.strip tokenM


writeToken :: Text -> IO ()
writeToken token = do
  elmHome <- Root.getElmHomePath
  writeUtf8 (elmHome </> tokenFile) token


removeToken :: IO ()
removeToken = do
  elmHome <- Root.getElmHomePath
  remove (elmHome </> tokenFile)


validateCliToken :: IO Text
validateCliToken = do
  appName <- Lamdera.Project.appNameOrThrow
  validateCliTokenFor (Just appName)


{-| validateCliToken for an app, or for none: a valid token, or the problem reported and
exit. -}
validateCliTokenFor :: Maybe Text -> IO Text
validateCliTokenFor appNameM = do
  result <- checkCliToken appNameM
  case result of
    Right token ->
      pure token

    Left problem ->
      case problem of
        CliAuthHttp error -> do
          Lamdera.Http.printHttpError error "I need to validate the CLI session"
          exitFailure

        _ -> do
          Progress.report $ D.fillSep ["───>", D.red (D.fromChars (T.unpack (cliAuthProblemText problem)))]
          exitFailure


data CliAuthProblem
  = CliAuthNoToken
  | CliAuthInvalid
  | CliAuthHttp Lamdera.Http.Error


cliAuthProblemText :: CliAuthProblem -> Text
cliAuthProblemText problem =
  case problem of
    CliAuthNoToken -> "No CLI auth, please run `lamdera login`"
    CliAuthInvalid -> "Invalid CLI auth, please re-run `lamdera login`"
    CliAuthHttp error -> "I could not reach the Lamdera dashboard to validate the CLI session: " <> T.pack (show error)


{-| The CLI token, checked with the dashboard, without printing anything. An invalid
token is removed, as `lamdera login` would replace it anyway. -}
checkCliToken :: Maybe Text -> IO (Either CliAuthProblem Text)
checkCliToken appNameM = do
  elmHome <- Root.getElmHomePath
  existingToken <- getToken

  case existingToken of
    Just token -> do
      apiSession <- fetchApiSessionFor appNameM token

      case apiSession of
        Right "success" -> do
          pure $ Right token

        Left error -> do
          case error of
            Lamdera.Http.HttpError httpExit -> do
              pure $ Left $ CliAuthHttp error

            _ -> do
              removeToken
              pure $ Left CliAuthInvalid

        _ -> do
          removeToken
          pure $ Left CliAuthInvalid


    Nothing -> do
      debug_ $ "Found no token in " <> elmHome
      pure $ Left CliAuthNoToken


fetchApiSession :: Text -> Text -> IO (Either Lamdera.Http.Error Text)
fetchApiSession appName token =
  fetchApiSessionFor (Just appName) token


{-| fetchApiSession for an app, or for none (`lamdera app list` and `app create` work
outside any app). The app name only picks the dashboard (see Lamdera.Http.dashboardFor). -}
fetchApiSessionFor :: Maybe Text -> Text -> IO (Either Lamdera.Http.Error Text)
fetchApiSessionFor appNameM token = do
  dashboard <- Lamdera.Http.dashboardFor appNameM
  let
    endpoint =
      Lamdera.Http.dashboardEndpoint dashboard "/_r/apiSessionJson"
      -- "https://" <> T.unpack appName <> ".lamdera.test/_i"

    body =
      E.object [ ("token", E.jsonString $ Json.String.fromChars $ T.unpack token) ]

    decoder =
      D.jsonString

  (fmap $ T.pack . Json.String.toChars) <$> Lamdera.Http.normalRpcJsonVia (Lamdera.Http._dashboardSocket dashboard) "fetchApiSession" body endpoint decoder


doUntil :: (a -> Bool) -> (IO a) -> IO ()
doUntil check fn = do
  x <- fn
  if check x
    then pure ()
    else doUntil check fn
