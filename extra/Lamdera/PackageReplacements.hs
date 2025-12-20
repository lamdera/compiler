{-# LANGUAGE TemplateHaskell #-}

module Lamdera.PackageReplacements where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import Data.ByteString


type ReplacementMap = Map.Map ( Pkg.Name, ModuleName.Raw ) ByteString


elm :: ReplacementMap
elm =
  -- TODO: Use TemplateHaskell to populate this map from extra/package-replacements/*
  -- Use `git diff master... --name-only` to know which files need to be included
  Map.empty


kernel :: ReplacementMap
kernel =
  -- TODO: Same here.
  Map.empty


get :: Pkg.Name -> ModuleName.Raw -> ReplacementMap -> IO ByteString -> IO ByteString
get pkg name replacementMap original =
  maybe original return (Map.lookup (pkg, name) replacementMap)


-- TODO: Also generate a map of package names and package versions used to validate that the
-- user elm.json is compatible with our package replacements.
