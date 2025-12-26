{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PackageReplacementsTH where

import Control.Exception (throwIO)
import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))
import qualified System.Process


loadReplacements :: TH.Q TH.Exp
loadReplacements = do
  submodules <- TH.runIO findSubmodules
  entries <- TH.runIO (collectEntriesPerSubmodule submodules)
  let entriesWithModuleNames = Maybe.mapMaybe parseModuleName entries
  listE <- mapM entryToExp entriesWithModuleNames
  [| Map.fromList $(pure (TH.ListE listE)) |]


loadVersions :: TH.Q TH.Exp
loadVersions = do
  submodules <- TH.runIO findSubmodules
  entries <- TH.runIO (collectVersionPerSubmodule submodules)
  listE <- mapM entry2ToExp entries
  [| $(pure (TH.ListE listE)) |]


findSubmodules :: IO [(String, String, FilePath)]
findSubmodules = do
  let root = "extra/package-replacements"
  authors <- Dir.listDirectory root
  fmap (List.sort . List.concat) $
    forM authors $ \author -> do
      let dir = root </> author
      projects <- Dir.listDirectory dir
      pure (fmap (\project -> (author, project, dir </> project)) projects)


gitChangedFilesIn :: FilePath -> IO [FilePath]
gitChangedFilesIn dir = do
  stdout <- System.Process.readProcess "git" ["-C", dir, "diff", "origin/master...", "--name-only"] ""
  pure (lines stdout)


gitCommitHashIn :: FilePath -> IO String
gitCommitHashIn dir = do
  stdout <- System.Process.readProcess "git" ["-C", dir, "rev-parse", "HEAD"] ""
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
  fmap List.concat $
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
      FP.takeExtension filePath
  in
  if ext == ".js" || ext == ".elm" then
    case FP.splitDirectories (FP.dropExtension filePath) of
      "extra" : "package-replacements" : _ : _ : "src" : rest ->
        Just (authorProject, filePath, List.intercalate "." rest)

      _ ->
        Nothing

  else
    Nothing


entryToExp
  :: ((String, String), FilePath, String)
  -> TH.Q TH.Exp
entryToExp ((author, project), path, moduleName) = do
  bytes <- TH.runIO (B.readFile path)

  [|
    ( ( Pkg.toName (Utf8.fromChars author) project
      , Name.fromChars moduleName
      )
    , $(Language.Haskell.TH.Syntax.lift bytes)
    )
   |]


entry2ToExp
  :: ((String, String), V.Version, String)
  -> TH.Q TH.Exp
entry2ToExp ((author, project), V.Version major minor patch, commit) =
  [|
    ( Pkg.toName (Utf8.fromChars author) project
    , V.Version major minor patch
    , commit
    )
   |]
