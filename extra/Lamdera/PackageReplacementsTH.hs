{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PackageReplacementsTH where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import Data.ByteString

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
  entries    <- runIO (collectEntriesPerSubmodule submodules)
  let entriesWithModuleNames = Data.Maybe.mapMaybe parseModuleName entries
  listE      <- mapM entryToExp entriesWithModuleNames
  [| Map.fromList $(pure (ListE listE)) |]

loadVersions :: Q Exp
loadVersions = do
  submodules <- runIO findSubmodules
  entries    <- runIO (collectVersionPerSubmodule submodules)
  listE      <- mapM entry2ToExp entries
  [| Map.fromList $(pure (ListE listE)) |]

findSubmodules :: IO [(String, String, FilePath)]
findSubmodules = do
  let root = "extra/package-replacements"
  authors <- listDirectory root
  fmap Data.List.concat $
    forM authors $ \author -> do
      let aDir = root </> author
      projects <- listDirectory aDir
      pure
        [ (author, project, aDir </> project)
        | project <- projects
        ]

gitChangedFilesIn :: FilePath -> IO [FilePath]
gitChangedFilesIn dir = do
  out <- readProcess
           "git"
           ["-C", dir, "diff", "origin/master...", "--name-only"]
           ""
  pure (lines out)

readVersion :: FilePath -> IO V.Version
readVersion fp = do
  bs <- Data.ByteString.readFile (fp </> "elm.json")
  case D.fromByteString (D.field "version" V.decoder) bs of
    Left _ ->
      throwIO (userError "nope")

    Right version ->
      return version

collectEntriesPerSubmodule
  :: [(String, String, FilePath)]
  -> IO [((String, String), FilePath)]
collectEntriesPerSubmodule subs =
  fmap Data.List.concat $
    forM subs $ \(author, project, dir) -> do
      changed <- gitChangedFilesIn dir
      pure
        [ ((author, project), dir </> fp)
        | fp <- changed
        ]

collectVersionPerSubmodule
  :: [(String, String, FilePath)]
  -> IO [((String, String), V.Version)]
collectVersionPerSubmodule subs =
  forM subs $ \(author, project, dir) -> do
    version <- readVersion dir
    pure
      ((author, project), version)

parseModuleName :: ((String, String), FilePath) -> Maybe ((String, String), FilePath, String)
parseModuleName (authorProject, filePath) =
  let
    ext = takeExtension filePath
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
  bs <- runIO (Data.ByteString.readFile path)

  [|
    ( ( Pkg.toName (Utf8.fromChars author) project
      , Name.fromChars moduleName
      )
    , $(lift bs)
    )
   |]

entry2ToExp
  :: ((String, String), V.Version)
  -> Q Exp
entry2ToExp ((author, project), version) =
  [|
    ( Pkg.toName (Utf8.fromChars author) project
    , $(liftVersion version)
    )
   |]

liftVersion :: V.Version -> Q Exp
liftVersion (V.Version major minor patch) =
  [| V.Version major minor patch |]
