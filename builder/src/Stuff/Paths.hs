{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Stuff.Paths
  ( docs
  , summary
  , prepublishDir
  , removeStuff
  , elmi
  , elmo
  , stuff
  , moduleDocs
  , temp
  , lamderaHashes
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
import qualified Data.Text as T


-- PATHS


stuff :: FilePath
stuff =
  "lamdera-stuff" </> Pkg.versionToString Compiler.version


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
  do  let dir = root </> "lamdera-stuff"
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


lamderaHashes :: FilePath -> FilePath
lamderaHashes root =
  root </> "lamdera-stuff" </> ".lamdera-hashes"
