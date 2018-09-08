{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Stuff.Paths
  ( docs
  , summary
  , prepublishDir
  , removeStuff
  , elmi
  , elmo
  , haskelmo
  , haskelmoWithoutStuff
  , haskellPackageYaml
  , haskellAppPackageYaml
  , cabalNameOfPackage
  , moduleDocs
  , temp
  )
  where


import Control.Monad.Trans (liftIO)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task
import Data.Monoid ((<>))


-- PATHS


stuff :: FilePath
stuff =
  "elm-stuff" </> Pkg.versionToString Compiler.version


docs :: FilePath
docs =
  stuff </> "docs.json"


summary :: FilePath
summary =
  stuff </> "summary.dat"


prepublishDir :: FilePath
prepublishDir  =
  stuff </> "prepublish"



-- REMOVE STUFF


removeStuff :: FilePath -> Task.Task_ e ()
removeStuff root =
  liftIO $
  do  let dir = root </> "elm-stuff"
      exists <- Dir.doesDirectoryExist dir
      if exists
        then Dir.removeDirectoryRecursive dir
        else return ()



-- ELMI and ELMO


elmi :: FilePath -> Module.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo :: FilePath -> Module.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"

haskelmo :: FilePath -> Module.Raw -> FilePath
haskelmo root name =
  --toArtifactPath root name "hs"
  haskelmoWithoutStuff (root </> stuff) name

haskelmoWithoutStuff :: FilePath -> Module.Raw -> FilePath
haskelmoWithoutStuff root name =
  root </> "haskelm" </> "src" </> Module.nameToSlashPath name <.> "hs"

haskellPackageYaml :: FilePath -> FilePath
haskellPackageYaml root =
  root </> "haskelm" </> "package" <.> "yaml"

haskellAppPackageYaml :: FilePath -> FilePath
haskellAppPackageYaml root =
  root </> "package" <.> "yaml"

cabalNameOfPackage (Pkg.Name author project) =
  author <> "-delim-" <> project

moduleDocs :: FilePath -> Module.Raw -> FilePath
moduleDocs root name =
  toArtifactPath root name "json"


toArtifactPath :: FilePath -> Module.Raw -> String -> FilePath
toArtifactPath root name ext =
  root </> stuff </> Module.nameToHyphenPath name <.> ext



-- TEMP


temp :: String -> FilePath
temp ext =
  stuff </> "temp" <.> ext
