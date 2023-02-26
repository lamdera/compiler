module Lamdera.RelativeLoad where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import Lamdera


-- These help with loading relative file paths from the compiler codebase when we're developing in GHCI and the current working directory isn't reliably set
-- This happens because the Elm compiler relies on the current working directory to find project roots, so in our test suite we end up needing to modify this value a fair bit
-- These helpers currently expect that the lamdera-compiler codebase is checked out to `~/dev/projects/lamdera-compiler`


find :: FilePath -> IO BS.ByteString
find path = do
  fullPath <- findFile path
  BS.readFile fullPath


findFile :: String -> IO String
findFile path = do
  fileExists <- doesFileExist path
  if fileExists
    then pure path
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      userHome <- Dir.getHomeDirectory
      let relativePath = userHome </> "dev/projects/lamdera-compiler" </> path

      exists2 <- doesFileExist relativePath
      if exists2
        then pure relativePath
        else
          error $ "could not find a relative path, seeking at:\n" <> path <> "\n" <> relativePath


findDir :: String -> IO String
findDir path = do
  fileExists <- doesDirectoryExist path
  if fileExists
    then Dir.makeAbsolute path
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      userHome <- Dir.getHomeDirectory
      let relativePath = userHome </> "dev/projects/lamdera-compiler" </> path

      exists2 <- doesDirectoryExist relativePath
      if exists2
        then pure relativePath
        else
          error $ "could not find a relative path, seeking at:\n" <> path <> "\n" <> relativePath
