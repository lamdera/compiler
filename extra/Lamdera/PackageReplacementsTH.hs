{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PackageReplacementsTH where

import qualified Data.Map as Map
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
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


loadReplacements :: Q Exp
loadReplacements = do
  submodules <- runIO findSubmodules
  entries    <- runIO (collectEntriesPerSubmodule submodules)
  listE      <- mapM entryToExp entries
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

bs :: ByteString
bs = "test"

entryToExp
  :: ((String, String), FilePath)
  -> Q Exp
entryToExp ((author, project), path) = do
  -- bs <- runIO (Data.ByteString.readFile path)

  -- let moduleName = dropExtension (takeFileName path)
  let moduleName = path

  [|
    ( ( Pkg.toName (Utf8.fromChars author) project
      , Name.fromChars moduleName
      )
    , $(lift bs)
    )
   |]
