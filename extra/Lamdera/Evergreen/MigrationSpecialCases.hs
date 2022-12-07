{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


module Lamdera.Evergreen.MigrationSpecialCases where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import NeatInterpolation

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Name as N

import Lamdera.Evergreen.MigrationGeneratorHelpers


specialCaseMigration identifier regularMigrationGen =
    case identifier of
      ("pzp1997", "assoc-list", "AssocList", "Dict") ->
        migration identifier
          [text|
            migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
            migrate_AssocList_Dict migrate_a migrate_b old =
                Dict.toList old
                    |> List.map (Tuple.mapBoth migrate_a migrate_b)
                    |> Dict.fromList
          |]

      ("mgold", "elm-nonempty-list", "List.Nonempty", "Nonempty") ->
        migration identifier
          [text|
            migrate_List_Nonempty_Nonempty : (a_old -> a_new) -> List.Nonempty.Nonempty a_old -> List.Nonempty.Nonempty a_new
            migrate_List_Nonempty_Nonempty migrate_a old =
                List.Nonempty.map migrate_a old
          |]

      _ ->
        regularMigrationGen

migration :: TypeIdentifier -> Text -> MigrationDefinition
migration (author, pkg, module_, typeName) def =
  MigrationDefinition
    { imports = Set.singleton $ ModuleName.Canonical (Pkg.Name author pkg) module_
    , migrations = Map.singleton ("migrate_" <> N.toText module_ <> "_" <> N.toText typeName) def
    }


