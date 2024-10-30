{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Update where

{- Helpers for updates
-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Text.Read

import qualified Json.String

import Lamdera
import qualified Lamdera.Http
import qualified Lamdera.Version

fetchCurrentVersion :: IO (Either Lamdera.Http.Error Text)
fetchCurrentVersion = do
  let
    endpoint =
      "https://static.lamdera.com/bin/latest-version.json"

    decoder =
      D.string

  fmap (T.pack . Json.String.toChars) <$> Lamdera.Http.normalJson "fetchCurrentVersion" endpoint decoder

getLatestVersion :: IO (Either String (Int, Int, Int))
getLatestVersion = do
  latestVersionText_ <- fetchCurrentVersion
  case latestVersionText_ of
    Right latestVersionText -> do
      let
        toIntCertain :: Text -> Int
        toIntCertain t =
          t & T.unpack
            & Text.Read.readMaybe
            & withDefault 0

        latestVersion =
          latestVersionText
            & T.splitOn "-"
            & (\parts ->
              case parts of
                ev:lv:_ ->
                  lv
                    & T.splitOn "."
                    & fmap toIntCertain
                    & (\parts_ ->
                      case parts_ of
                        [v1, v2, v3] ->
                          Right ( v1, v2, v3 )

                        _ ->
                          Left "Got an invalid lamdera version format"
                    )

                _ -> Left "Got an invalid full version format"
            )
      pure latestVersion
    Left err ->
      pure $ Left $ show err


-- Check if our current version is the latest
isLatest :: Lamdera.Version.Version -> Bool
isLatest latestVersion =
  let
    outOfDate = Lamdera.Version.raw < latestVersion
    isOnLatest = not outOfDate
  in
  isOnLatest
