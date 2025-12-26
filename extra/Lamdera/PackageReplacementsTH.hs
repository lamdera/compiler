{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PackageReplacementsTH where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Data.ByteString as B

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath
import System.Process
import Control.Monad
import Data.List
import qualified Data.Utf8 as Utf8
import qualified Data.Name as Name
import qualified Data.Maybe
import qualified Json.Decode as D
import Control.Exception (throwIO)


loadReplacements :: Q Exp
loadReplacements = do
  submodules <- runIO findSubmodules
  entries <- runIO (collectEntriesPerSubmodule submodules)
  let entriesWithModuleNames = Data.Maybe.mapMaybe parseModuleName entries
  listE <- mapM entryToExp entriesWithModuleNames
  [| Map.fromList $(pure (ListE listE)) |]


loadVersions :: Q Exp
loadVersions = do
  submodules <- runIO findSubmodules
  entries <- runIO (collectVersionPerSubmodule submodules)
  listE <- mapM entry2ToExp entries
  [| $(pure (ListE listE)) |]


findSubmodules :: IO [(String, String, FilePath)]
findSubmodules = do
  let root = "extra/package-replacements"
  authors <- listDirectory root
  fmap (Data.List.sort . Data.List.concat) $
    forM authors $ \author -> do
      let dir = root </> author
      projects <- listDirectory dir
      pure (fmap (\project -> (author, project, dir </> project)) projects)


gitChangedFilesIn :: FilePath -> IO [FilePath]
gitChangedFilesIn dir = do
  stdout <- readProcess "git" ["-C", dir, "diff", "origin/master...", "--name-only"] ""
  pure (lines stdout)


gitCommitHashIn :: FilePath -> IO String
gitCommitHashIn dir = do
  stdout <- readProcess "git" ["-C", dir, "rev-parse", "HEAD"] ""
  pure (init stdout) -- Drop trailing newline.


readVersion :: FilePath -> IO V.Version
readVersion dir = do
  let elmJsonPath = dir </> "elm.json"
  bytes <- B.readFile elmJsonPath
  case D.fromByteString (D.field "version" V.decoder) bytes of
    Left _ ->
      throwIO (userError ("Failed to decode version from: " <> elmJsonPath))

    Right version ->
      return version


collectEntriesPerSubmodule
  :: [(String, String, FilePath)]
  -> IO [((String, String), FilePath)]
collectEntriesPerSubmodule subs =
  fmap Data.List.concat $
    forM subs $ \(author, project, dir) -> do
      changed <- gitChangedFilesIn dir
      pure (fmap (\fp -> ((author, project), dir </> fp)) changed)


collectVersionPerSubmodule
  :: [(String, String, FilePath)]
  -> IO [((String, String), V.Version, String)]
collectVersionPerSubmodule subs =
  forM subs $ \(author, project, dir) -> do
    version <- readVersion dir
    commit <- gitCommitHashIn dir
    pure ((author, project), version, commit)


parseModuleName :: ((String, String), FilePath) -> Maybe ((String, String), FilePath, String)
parseModuleName (authorProject, filePath) =
  let
    ext =
      takeExtension filePath
  in
  if ext == ".js" || ext == ".elm" then
    case splitDirectories (dropExtension filePath) of
      "extra" : "package-replacements" : _ : _ : "src" : rest ->
        Just (authorProject, filePath, Data.List.intercalate "." rest)

      _ ->
        Nothing

  else
    Nothing


entryToExp
  :: ((String, String), FilePath, String)
  -> Q Exp
entryToExp ((author, project), path, moduleName) = do
  bytes <- runIO (B.readFile path)

  [|
    ( ( Pkg.toName (Utf8.fromChars author) project
      , Name.fromChars moduleName
      )
    , $(lift bytes)
    )
   |]


entry2ToExp
  :: ((String, String), V.Version, String)
  -> Q Exp
entry2ToExp ((author, project), V.Version major minor patch, commit) =
  [|
    ( Pkg.toName (Utf8.fromChars author) project
    , V.Version major minor patch
    , commit
    )
   |]
