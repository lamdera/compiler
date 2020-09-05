{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Hints where

import qualified Reporting.Doc as D
import qualified Elm.Package as Pkg
import Elm.ModuleName (Canonical(..))
import qualified Type.Error as T
import StandaloneInstances

import Data.Monoid ((<>))
import qualified Debug.Trace as DT

contextHintsWhenTypeMismatch tipe =
  case tipe of
    -- @TODO fix when we move this to core
    (T.Type (Canonical (Pkg.Name "author" "project") "Evergreen.Migrate") "UnimplementedMigration" []) ->
        [ D.toSimpleHint $
           "I need you to implement migrations for changed types\
            \ as described in <https://dashboard.lamdera.app/docs/evergreen>"
        ]
    _ ->
      []
