{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen.MigrationGenerator where

-- import Prelude hiding (init)
-- import qualified System.Directory as Dir
import qualified Data.Text as T
import NeatInterpolation
-- import Algorithms.NaturalSort
-- import qualified Data.List as List
-- import System.FilePath ((</>))
-- import Control.Monad.Except (catchError, liftIO)
-- import Text.Read (readMaybe)
import qualified Data.Set as Set
import qualified Elm.Interface as Interface
-- import Data.Maybe (fromMaybe)
-- import Data.List
import qualified Data.Map as Map

import qualified Data.Name as N

import Lamdera
import qualified Lamdera.Compile
import qualified Ext.Query.Interfaces
import qualified Ext.ElmFormat

import qualified Lamdera.Evergreen.MigrationGenX


generateFor typename oldVersion newVersion = do

    let
        project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"
        paths = ["src/Evergreen/V" <> show oldVersion <> "/Types.elm", "src/Evergreen/V" <> show newVersion <> "/Types.elm"]

    Lamdera.Compile.makeDev project paths


    res <- withCurrentDirectory project $ do
        interfaces <- Ext.Query.Interfaces.all paths

        Lamdera.Evergreen.MigrationGenX.dothewholething oldVersion newVersion interfaces (interfaces Map.! (N.fromChars $ "Evergreen.V" <> show newVersion <> ".Types"))

        -- let
        --   -- !_ = debugHaskell "keys" (Map.keys interfaces)
        --   typename = "BackendModel"
        --   modulename = "Evergreen.V1.Types"
        --   interfaceModule = (interfaces Map.! modulename)

        -- pure ""
          -- typeOld = sourceType interfaces typename modulename
          -- typeNew = sourceType interfaces typename modulename

        -- typeDiffMigration interfaces typename modulename interfaceModule

        -- pure interfaces

    pure res


-- @TODO remove unused
sourceType interfaces targetName moduleName  = do
  let
    interface = interfaces Map.! moduleName
    recursionSet = Set.singleton (moduleName, targetName, [])

  case Map.lookup targetName $ Interface._aliases interface of
    Just alias -> do
      Left alias
      -- aliasToDiffableType targetName interfaces recursionSet [] alias []

    Nothing ->
      -- Try unions
      case Map.lookup targetName $ Interface._unions interface of
        Just union ->
          Right union
          -- unionToDiffableType targetName (nameToText targetName) interfaces recursionSet [] union []

        Nothing ->
          error $ show $ "Found no type named " <> nameToText targetName <> " in " <> nameToText moduleName


betweenVersions :: Int -> Int -> [(String, String, String)] -> IO Text
betweenVersions oldVersion newVersion typeCompares = do
  let old = show_ oldVersion
      new = show_ newVersion

      typeCompareMigration :: (String, String, String) -> IO Text
      typeCompareMigration (typename, oldhash, newhash) = do
        implementation <- generateFor typename oldVersion newVersion
        let
            msgType = msgForType typename

            typenameCamel = lowerFirstLetter typename

            typenameT = T.pack typename

            migrationType = migrationWrapperForType typename

        pure implementation

      migrationWrapperForType t =
        case t of
          "BackendModel" -> "ModelMigration"
          "FrontendModel" -> "ModelMigration"
          "FrontendMsg" -> "MsgMigration"
          "ToBackend" -> "MsgMigration"
          "BackendMsg" -> "MsgMigration"
          "ToFrontend" -> "MsgMigration"

      msgForType t =
        case t of
          "BackendModel" -> "BackendMsg"
          "FrontendModel" -> "FrontendMsg"
          "FrontendMsg" -> "FrontendMsg"
          "ToBackend" -> "BackendMsg"
          "BackendMsg" -> "BackendMsg"
          "ToFrontend" -> "FrontendMsg"

      unchangedForType t =
        case t of
          "BackendModel" -> "ModelUnchanged"
          "FrontendModel" -> "ModelUnchanged"
          "FrontendMsg" -> "MsgUnchanged"
          "ToBackend" -> "MsgUnchanged"
          "BackendMsg" -> "MsgUnchanged"
          "ToFrontend" -> "MsgUnchanged"

  res <- typeCompares & mapM typeCompareMigration
  res
    & T.intercalate "\n\n\n"
    & Ext.ElmFormat.formatOrPassthrough
    -- & pure
