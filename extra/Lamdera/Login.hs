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

  token <- UUID.toText <$> UUID.nextRandom
  inProduction <- Lamdera.Project.inProduction
  appName <- Lamdera.Project.maybeAppName

  doUntil ((==) True) $
    checkApiLoop inProduction (appName & fromMaybe "test-local") token

  pure ()


doUntil :: (a -> Bool) -> (IO a) -> IO a
doUntil check fn = do
  x <- fn
  if check x
    then pure x
    else doUntil check fn


checkApiLoop inProduction appName token = do
  let username = "mario@cofoundry.uk"
  apiSession <- fetchApiSession appName token username True
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


fetchApiSession :: Text -> Text -> Text -> Bool -> IO (Either Reporting.Exit.Exit Text)
fetchApiSession appName token username useLocal =
  let
    endpoint =
      if textContains "-local" appName && useLocal
        then
          "http://localhost:8000/_r/apiSessionJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://" <> T.unpack appName <> ".lamdera.app/_r/apiSessionJson"

    body =
      E.object
        -- @TODO this cannot be in source
        [ ("token", E.text token)
        , ("username", E.text username)
        ]

    decoder =
      D.text

  in
  Lamdera.Http.tryNormalRpcJson body endpoint decoder
