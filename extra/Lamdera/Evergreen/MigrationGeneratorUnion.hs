{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Evergreen.MigrationGeneratorUnion where

import qualified AST.Canonical as Can
import qualified AST.Source as Valid
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import qualified Reporting.Error as Error

import qualified Reporting.Doc as D

import qualified System.Environment as Env
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N
import Data.Map.Strict (unionWithKey)

import qualified Data.Utf8 as Utf8

import Lamdera
import Lamdera.Types
import qualified Ext.Query.Interfaces as Interfaces
import qualified Lamdera.Progress as Progress
import qualified Ext.ElmFormat
import qualified Lamdera.Wire3.Helpers
import StandaloneInstances
import Lamdera.Evergreen.MigrationGeneratorHelpers



newConstructorWarnings :: N.Name -> Text -> Can.Union -> Can.Union -> [Text]
newConstructorWarnings typeName moduleScope newUnion oldUnion =
  Can._u_alts newUnion
    & filterMap (\(Can.Ctor newConstructor index int newParams) -> do
      case Can._u_alts oldUnion & List.find (\(Can.Ctor oldConstructor _ _ _) -> newConstructor == oldConstructor ) of
        Nothing ->
          let params =
                if length newParams > 0
                  then " " <> (T.intercalate " " (fmap asTypeName newParams))
                  else ""
          in
          -- This constructor is missing a match in the old type, warn the user this new constructor exists
          Just $ T.concat [
            "        {- `", N.toText newConstructor, params, "` doesn't exist in the old ", moduleScope, N.toText typeName, ".\n",
            "        This is just a reminder in case migrating some subset of the old data to this new value was important.\n",
            "        See https://lamdera.com/tips/modified-custom-type for more info. -}\n"
            ]
        Just _ ->
          -- This constructor has a match in the old type, so skip it
          Nothing
    )
    & (\notices ->
          if length notices > 0 then
            ["    notices ->\n" <>
            "        " <> T.concat notices <> "\n" <>
            "        Unimplemented"]
          else
            []
      )
