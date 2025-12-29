{-# LANGUAGE TemplateHaskell #-}

module Lamdera.PackageReplacements
  ( replacementMap
  , getReplacement
  , versions
  , getVersion
  , virtualDomVersion
  , virtualDomConstraint
  ) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as Map
import qualified Elm.Constraint as C
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Lamdera.PackageReplacementsTH as PackageReplacementsTH


replacementMap :: Map.Map ( Pkg.Name, ModuleName.Raw ) B.ByteString
replacementMap =
  $(PackageReplacementsTH.loadReplacements)


getReplacement :: Pkg.Name -> ModuleName.Raw -> IO B.ByteString -> IO B.ByteString
getReplacement pkg name original =
  maybe original return (Map.lookup (pkg, name) replacementMap)


type Commit = String


versions :: Map.Map Pkg.Name ( V.Version, Commit )
versions =
  $(PackageReplacementsTH.loadVersions)


getVersion :: ( Pkg.Name, C.Constraint ) -> ( Pkg.Name, C.Constraint )
getVersion ( name, originalConstraint ) =
  ( name
  , case Map.lookup name versions of
      Just ( version, _ ) ->
        C.exactly version

      Nothing ->
        originalConstraint
  )


virtualDomVersion :: Maybe V.Version
virtualDomVersion =
  fst <$> Map.lookup Pkg.virtualDom versions


virtualDomConstraint :: C.Constraint
virtualDomConstraint =
  maybe C.anything C.exactly virtualDomVersion
