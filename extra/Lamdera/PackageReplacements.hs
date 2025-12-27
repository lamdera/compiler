{-# LANGUAGE TemplateHaskell #-}

module Lamdera.PackageReplacements (replacementMap, get, versions) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Lamdera.PackageReplacementsTH as PackageReplacementsTH


replacementMap :: Map.Map ( Pkg.Name, ModuleName.Raw ) B.ByteString
replacementMap =
  $(PackageReplacementsTH.loadReplacements)


get :: Pkg.Name -> ModuleName.Raw -> IO B.ByteString -> IO B.ByteString
get pkg name original =
  maybe original return (Map.lookup (pkg, name) replacementMap)


type Commit = String


versions :: [ ( Pkg.Name, V.Version, Commit ) ]
versions =
  $(PackageReplacementsTH.loadVersions)
