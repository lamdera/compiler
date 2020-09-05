{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.OldFeatureHelper where

{- Added to help people missing LocalDev deps to have a nicer error message, as
being exposed to 'LocalDev.elm has a bad import" is quite confusing.

More context:

https://trello.com/c/j2igqmlo/497-yak-nice-error-handling-for-additional-package-deps

-}

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Elm.ModuleName as Module
import Data.Char (toLower)
import Data.List (isInfixOf)

import Lamdera

-- @LAMDERA integration needed – unclear where integration point from last
-- version has moved given error texts appear to have been updated. Reproduce the failure
-- case when LocalDev.elm is a thing again, then consider putting this in.
adviseLocalDevImport path child origin parent normalReport = do
  let
    n = Module.toChars child
    package =
      case n of
        "Time" -> "elm/time"
        "Json.Encode" -> "elm/json"
        "Json.Decode" -> "elm/json"
        "Http" -> "elm/http"
        _ -> ""


  if   (isInfixOf "LocalDev.elm" path && n == "Time")
    || (isInfixOf "LocalDev.elm" path && n == "Json.Encode")
    || (isInfixOf "LocalDev.elm" path && n == "Json.Decode")
    || (isInfixOf "LocalDev.elm" path && n == "Http")
    then do

      Help.report "MISSING PACKAGE" (Nothing)
        ("`lamdera live` needs certain packages to work.")
        [ D.reflow "Please run this command to add that dependency to your elm.json file:"
        , D.indent 4 $ D.green $ D.fromChars $ "lamdera install " <> package
        , D.reflow "Note: You're seeing this because it looks like you're upgrading from an older version of the Lamdera Alpha. If not, please report this issue!"
        ]

    else
      normalReport
