{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Update where

{- Helpers for updates
-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Lamdera.Http
import qualified Json.String


fetchCurrentVersion :: IO (Either Lamdera.Http.Error Text)
fetchCurrentVersion = do
  let
    endpoint =
      "https://static.lamdera.com/bin/latest-version.json"

    decoder =
      D.string

  fmap (T.pack . Json.String.toChars) <$> Lamdera.Http.normalJson "fetchCurrentVersion" endpoint decoder
