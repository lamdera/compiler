{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Update where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Json.Decode as D
import qualified Reporting.Task as Task
import qualified Lamdera.Http


fetchCurrentVersion :: Task.Task Text
fetchCurrentVersion = do
  let
    endpoint =
      "https://lamdera.com/current-version.json"

    decoder =
      D.text

  Lamdera.Http.normalJson "fetchAppConfigItems" endpoint decoder
