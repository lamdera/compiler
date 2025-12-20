{-# LANGUAGE TemplateHaskell #-}

module Lamdera.PackageReplacements where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import Data.ByteString


type ReplacementMap = Map.Map ( Pkg.Name, ModuleName.Raw ) ByteString


elm :: ReplacementMap
elm = Map.empty


kernel :: ReplacementMap
kernel = Map.empty


get :: Pkg.Name -> ModuleName.Raw -> ReplacementMap -> IO ByteString -> IO ByteString
get pkg name replacementMap original =
  maybe original return (Map.lookup (pkg, name) replacementMap)
