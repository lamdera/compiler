{-# OPTIONS_GHC -Wall #-}
module Elm.PerUserCache
  ( getPackageRoot
  , getReplRoot
  , getElmHome
  )
  where

import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg



-- ROOTS


getPackageRoot :: IO FilePath
getPackageRoot =
  getRoot "package"


getReplRoot :: IO FilePath
getReplRoot =
  getRoot "repl"


getRoot :: FilePath -> IO FilePath
getRoot projectName =
  do  home <- getElmHome
      let root = home </> version </> projectName
      Dir.createDirectoryIfMissing True root
      return root


version :: FilePath
version =
  Pkg.versionToString Compiler.version


getElmHome :: IO FilePath
getElmHome =
  do  maybeHome <- Env.lookupEnv "ELM_HOME"
      maybe (Dir.getAppUserDataDirectory "elm") return maybeHome
      -- @LAMDERA override
      maybe ((\v -> v </> "elm") <$> Dir.getAppUserDataDirectory "lamdera") pure maybeHome
      -- @LAMDERA revisit for IDE support: keep the root so the mapping doesn't have to be `lamdera-stuff/elm`
      -- and use LAMDERA_HOME so that choice doesn't cause clashes with usage of ELM_HOME
      -- maybeHomeLamdera <- Env.lookupEnv "LAMDERA_HOME"
      -- maybe (Dir.getAppUserDataDirectory "lamdera") pure maybeHomeLamdera
