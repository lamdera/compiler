{-# LANGUAGE TemplateHaskell #-}

module Lamdera.PackageReplacements where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import Data.ByteString
import Lamdera.PackageReplacementsTH (loadReplacements, loadVersions)
import qualified Elm.Version as V


replacementMap :: Map.Map ( Pkg.Name, ModuleName.Raw ) ByteString
replacementMap =
  $(loadReplacements)


get :: Pkg.Name -> ModuleName.Raw -> IO ByteString -> IO ByteString
get pkg name original =
  maybe original return (Map.lookup (pkg, name) replacementMap)


versions :: Map.Map Pkg.Name V.Version
versions =
  $(loadVersions)
