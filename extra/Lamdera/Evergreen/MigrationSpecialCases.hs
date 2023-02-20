{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


module Lamdera.Evergreen.MigrationSpecialCases where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import NeatInterpolation

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Name as N

import Lamdera.Evergreen.MigrationGeneratorHelpers
import Lamdera


specialCaseMigration :: TypeIdentifier -> Maybe MigrationDefinition
specialCaseMigration identifier =
    case identifier of
      ("pzp1997", "assoc-list", "AssocList", "Dict") ->
        migrationDefinition identifier
          [text|
            migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
            migrate_AssocList_Dict migrate_a migrate_b old =
                old
                    |> AssocList.toList
                    |> List.map (Tuple.mapBoth migrate_a migrate_b)
                    |> AssocList.fromList
          |]

      ("erlandsona", "assoc-set", "AssocSet", "Set") ->
        migrationDefinition identifier
          [text|
            migrate_AssocSet_Set : (a_old -> a_new) -> AssocSet.Set a_old -> AssocSet.Set a_new
            migrate_AssocSet_Set migrate_a old =
                old |> AssocSet.map migrate_a
          |]

      ("mgold", "elm-nonempty-list", "List.Nonempty", "Nonempty") ->
        migrationDefinition identifier
          [text|
            migrate_List_Nonempty_Nonempty : (a_old -> a_new) -> List.Nonempty.Nonempty a_old -> List.Nonempty.Nonempty a_new
            migrate_List_Nonempty_Nonempty migrate_a old =
                old |> List.Nonempty.map migrate_a
          |]

      ("ianmackenzie", "elm-units", "Quantity", "Quantity") ->
        migrationDefinition identifier
          [text|
            migrate_Quantity_Quantity : Quantity.Quantity number units -> Quantity.Quantity number units2
            migrate_Quantity_Quantity old =
                Quantity.unwrap old |> Quantity.unsafe
          |]

      ("ianmackenzie", "elm-triangular-mesh", "TriangularMesh", "TriangularMesh") ->
        migrationDefinition identifier
          [text|
            migrate_TriangularMesh_TriangularMesh : (vertex_old -> vertex_new) -> TriangularMesh.TriangularMesh vertex_old -> TriangularMesh.TriangularMesh vertex_new
            migrate_TriangularMesh_TriangularMesh migrate_vertex old =
                old |> TriangularMesh.mapVertices migrate_vertex
          |]

      ("MartinSStewart", "elm-audio", "Audio", "Msg") ->
        migrationDefinition identifier
          [text|
            migrate_Audio_Msg : (userMsg_old -> userMsg_new) -> Audio.Msg userMsg_old -> Audio.Msg userMsg_new
            migrate_Audio_Msg migrate_userMsg old =
                old
                    |> Audio.migrateMsg (\userMsg_old -> (migrate_userMsg userMsg_old, Cmd.none))
                    |> Tuple.first
          |]

      ("MartinSStewart", "elm-audio", "Audio", "Model") ->
        migrationDefinition identifier
          [text|
            migrate_Audio_Model : (userMsg_old -> userMsg_new) -> (userModel_old -> userModel_new) -> Audio.Model userMsg_old userModel_old -> Audio.Model userMsg_new userModel_new
            migrate_Audio_Model migrate_userMsg migrate_userModel old =
                old
                    |> Audio.migrateModel migrate_userMsg (\userModel_old -> (migrate_userModel userModel_old, Cmd.none))
                    |> Tuple.first
          |]

      ("edkelly303", "elm-any-type-collections", "Any.Dict", "Dict") ->
          migrationDefinition identifier
          [text|
            migrate_Any_Dict_Dict :
                (k_old -> k_new)
                -> (v_old -> v_new)
                -> (comparable_old -> comparable_new)
                ->
                    { interface_old : Any.Dict.Interface k_old v_old v2_old output_old comparable_old
                    , interface_new : Any.Dict.Interface k_new v_new v2_new output_new comparable_new
                    }
                -> Any.Dict.Dict k_old v_old comparable_old
                -> Any.Dict.Dict k_new v_new comparable_new
            migrate_Any_Dict_Dict migrate_k migrate_v migrate_comparable { interface_old, interface_new } old =
                old
                    |> interface_old.toList
                    |> List.map (Tuple.mapBoth migrate_k migrate_v)
                    |> interface_new.fromList
          |]

      _ ->
        Nothing


migrationDefinition :: TypeIdentifier -> Text -> Maybe MigrationDefinition
migrationDefinition (author, pkg, module_, typeName) def =
  let marker = (T.concat ["migrate_", module_ & N.toText & T.replace "." "_", "_", N.toText typeName])
  in
  Just $ MigrationDefinition
    { migrationDefImports = Set.singleton $ ModuleName.Canonical (Pkg.Name author pkg) module_
    , migrationDef = def
    }
