{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Update where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Reporting.Task as Task
import qualified Lamdera.Http
import qualified Json.String


fetchCurrentVersion :: IO (Either Lamdera.Http.Error Json.String.String)
fetchCurrentVersion = do
  let
    endpoint =
      "https://lamdera.com/current-version.json"

    decoder =
      D.string

  Lamdera.Http.normalJson "fetchAppConfigItems" endpoint decoder
