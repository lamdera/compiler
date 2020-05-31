module Lamdera.OldFeatureHelper where

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Elm.Compiler.Module as Module
import Data.Char (toLower)
import Data.List (isInfixOf)

import Lamdera

adviseLocalDevImport path child normalReport = do
  let
    n = Module.nameToString child

  if isInfixOf "LocalDev.elm" path && n == "Time"
    then
      Help.report "MISSING PACKAGE" (Nothing)
        ("`lamdera live` needs certain packages to work.")
        [ D.reflow "Please run this command to add that dependency to your elm.json file:"
        , D.indent 4 $ D.green $ D.fromString $ "lamdera install elm/time"
        , D.reflow "Note: You're seeing this because it looks like you're upgrading from an older version of the Lamdera Alpha. If not, please report this issue!"
        ]

    else
      normalReport
