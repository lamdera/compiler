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


specialCaseMigration :: TypeIdentifier -> Text -> [Migration] -> Maybe (Text, MigrationDefinition, [Migration])
specialCaseMigration identifier genFn tvarMigrations =
    case identifier of
      ("elm-explorations", "webgl", "WebGL", "Mesh") ->
        migrationDefinition identifier
        "migrate_WebGL_Mesh"
        -- @TODO this will break things – but how else should we migrate it?
        [text|
          migrate_WebGL_Mesh : WebGL.Mesh attributes_old -> WebGL.Mesh attributes_new
          migrate_WebGL_Mesh old =
            WebGL.triangles []
        |]
        noTvarMigrations

      ("pzp1997", "assoc-list", "AssocList", "Dict") ->
        migrationDefinition identifier genFn
          [text|
            migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
            migrate_AssocList_Dict migrate_a migrate_b old =
                old
                    |> AssocList.toList
                    |> List.map (Tuple.mapBoth migrate_a migrate_b)
                    |> AssocList.fromList
          |]
          tvarMigrations

      ("erlandsona", "assoc-set", "AssocSet", "Set") ->
        migrationDefinition identifier genFn
          [text|
            migrate_AssocSet_Set : (a_old -> a_new) -> AssocSet.Set a_old -> AssocSet.Set a_new
            migrate_AssocSet_Set migrate_a old =
                old |> AssocSet.map migrate_a
          |]
          tvarMigrations

      ("mgold", "elm-nonempty-list", "List.Nonempty", "Nonempty") ->
        migrationDefinition identifier genFn
          [text|
            migrate_List_Nonempty_Nonempty : (a_old -> a_new) -> List.Nonempty.Nonempty a_old -> List.Nonempty.Nonempty a_new
            migrate_List_Nonempty_Nonempty migrate_a old =
                old |> List.Nonempty.map migrate_a
          |]
          tvarMigrations

      ("ianmackenzie", "elm-units", "Quantity", "Quantity") ->
        migrationDefinition identifier
          -- This is a phantom type, so we don't need to migrate any values
          "migrate_Quantity_Quantity"
          [text|
            migrate_Quantity_Quantity : Quantity.Quantity number units -> Quantity.Quantity number units2
            migrate_Quantity_Quantity old =
                Quantity.unwrap old |> Quantity.unsafe
          |]
          noTvarMigrations

      ("ianmackenzie", "elm-triangular-mesh", "TriangularMesh", "TriangularMesh") ->
        migrationDefinition identifier genFn
          [text|
            migrate_TriangularMesh_TriangularMesh : (vertex_old -> vertex_new) -> TriangularMesh.TriangularMesh vertex_old -> TriangularMesh.TriangularMesh vertex_new
            migrate_TriangularMesh_TriangularMesh migrate_vertex old =
                old |> TriangularMesh.mapVertices migrate_vertex
          |]
          tvarMigrations

      ("MartinSStewart", "elm-audio", "Audio", "Msg") ->
        migrationDefinition identifier genFn
          [text|
            migrate_Audio_Msg : (userMsg_old -> userMsg_new) -> Audio.Msg userMsg_old -> Audio.Msg userMsg_new
            migrate_Audio_Msg migrate_userMsg old =
                old
                    |> Audio.migrateMsg (\userMsg_old -> (migrate_userMsg userMsg_old, Cmd.none))
                    |> Tuple.first
          |]
          tvarMigrations

      ("MartinSStewart", "elm-audio", "Audio", "Model") ->
        migrationDefinition identifier genFn
          [text|
            migrate_Audio_Model : (userMsg_old -> userMsg_new) -> (userModel_old -> userModel_new) -> Audio.Model userMsg_old userModel_old -> Audio.Model userMsg_new userModel_new
            migrate_Audio_Model migrate_userMsg migrate_userModel old =
                old
                    |> Audio.migrateModel migrate_userMsg (\userModel_old -> (migrate_userModel userModel_old, Cmd.none))
                    |> Tuple.first
          |]
          tvarMigrations

      ("edkelly303", "elm-any-type-collections", "Any.Dict", "Dict") ->
          migrationDefinition identifier
          (T.concat [ genFn, " ",
            [text|
              { interface_old =
                  Any.Dict.makeInterface
                      { fromComparable = Unimplemented {- I need you to write this implementation. -}
                      , toComparable = Unimplemented {- I need you to write this implementation. -}
                      }
              , interface_new =
                  Any.Dict.makeInterface
                      { fromComparable = Unimplemented {- I need you to write this implementation. -}
                      , toComparable = Unimplemented {- I need you to write this implementation. -}
                      }
              }
            |]
          ]
          )
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
          tvarMigrations

      _ ->
        Nothing


migrationDefinition :: TypeIdentifier -> Text -> Text -> [Migration] -> Maybe (Text, MigrationDefinition, [Migration])
migrationDefinition (author, pkg, module_, typeName) fn def tvarMigrations =
  Just
    ( fn
    , MigrationDefinition
        { migrationDefImports = Set.singleton $ ModuleName.Canonical (Pkg.Name author pkg) module_
        , migrationDef = def
        }
    , tvarMigrations
    )


noTvarMigrations :: [Migration]
noTvarMigrations = []
