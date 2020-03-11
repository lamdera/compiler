{-# LANGUAGE OverloadedStrings #-}

module LamderaHints where

import qualified Reporting.Doc as D
import qualified Elm.Package as Pkg
import AST.Module.Name (Canonical(..))
import qualified Type.Error as T


import Data.Monoid ((<>))
import qualified Debug.Trace as DT
import Lamdera



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
