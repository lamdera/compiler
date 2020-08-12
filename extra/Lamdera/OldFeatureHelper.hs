{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.OldFeatureHelper where

{- UNUSED

Kept for reference. Search `Lamdera.OldFeatureHelper` to see commented-out inclusion points.

Was added to help folks missing LocalDev deps to elm/http & elm/time have a nicer error
message for that particular case, but in the end decided better to have Lamdera.Debug expose
those bits and it remain an indirect dependency.

More context:

https://trello.com/c/j2igqmlo/497-yak-nice-error-handling-for-additional-package-deps

-}

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Elm.Compiler.Module as Module
import Data.Char (toLower)
import Data.List (isInfixOf)

import Lamdera

adviseLocalDevImport path child origin parent normalReport = do
  let
    n = Module.nameToString child
    package =
      case n of
        "Time" -> "elm/time"
        "Json.Encode" -> "elm/json"
        "Json.Decode" -> "elm/json"

  if   (isInfixOf "LocalDev.elm" path && n == "Time")
    || (isInfixOf "LocalDev.elm" path && n == "Json.Encode")
    || (isInfixOf "LocalDev.elm" path && n == "Json.Decode")
    || (isInfixOf "LocalDev.elm" path && n == "Http")
    then do

      let
        !_ = debugHaskell "other args" (origin, parent)

      Help.report "MISSING PACKAGE" (Nothing)
        ("`lamdera live` needs certain packages to work.")
        [ D.reflow "Please run this command to add that dependency to your elm.json file:"
        , D.indent 4 $ D.green $ D.fromString $ "lamdera install " <> package
        , D.reflow "Note: You're seeing this because it looks like you're upgrading from an older version of the Lamdera Alpha. If not, please report this issue!"
        ]

    else
      normalReport
