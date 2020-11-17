{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lamdera.Hints where

import qualified Reporting.Doc as D
import qualified Elm.Package as Pkg
import Elm.ModuleName (Canonical(..))
import qualified Type.Error as T

import Data.Monoid ((<>))
import qualified Debug.Trace as DT


contextHintsWhenTypeMismatch tipe =
  case tipe of
    -- @TODO fix when we move this to core
    (T.Type (Canonical package "Evergreen.Migrate") "UnimplementedMigration" []) ->
      if package == Pkg.dummyName
        then
          [ D.toSimpleHint $
             "I need you to implement migrations for changed types\
              \ as described in <https://dashboard.lamdera.app/docs/evergreen>"
          ]
        else
          []
    _ ->
      []
