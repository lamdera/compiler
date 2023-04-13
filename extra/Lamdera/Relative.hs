module Lamdera.Relative where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import Lamdera


-- These help with loading relative file paths from the compiler codebase when we're developing in GHCI and the current working directory isn't reliably set
-- This happens because the Elm compiler relies on the current working directory to find project roots, so in our test suite we end up needing to modify this value a fair bit
-- These helpers currently expect that the lamdera-compiler codebase is checked out to `~/dev/projects/lamdera-compiler`


readByteString :: FilePath -> IO BS.ByteString
readByteString path = do
  fullPath <- requireFile path
  BS.readFile fullPath


findFile :: String -> IO (Maybe String)
findFile path = do
  fileExists <- doesFileExist path
  if fileExists
    then pure (Just path)
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      userHome <- Dir.getHomeDirectory
      let absPath = userHome </> "dev/projects/lamdera-compiler" </> path

      exists2 <- doesFileExist absPath
      if exists2
        then pure (Just absPath)
        else
          pure Nothing


loadFile :: String -> IO (Maybe Text)
loadFile path = do
  found <- findFile path
  case found of
    Just absPath -> Lamdera.readUtf8Text absPath
    Nothing -> pure Nothing


requireFile :: String -> IO String
requireFile path = do
  found <- findFile path
  -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
  -- Only thing we can really do now is guess from a standard-ish location relative to home
  userHome <- Dir.getHomeDirectory
  let absPath = userHome </> "dev/projects/lamdera-compiler" </> path
  case found of
    Just absPath -> pure absPath
    Nothing -> error $ "could not find a relative path, seeking at:\n" <> path <> "\n" <> absPath


findDir :: String -> IO String
findDir path = do
  fileExists <- doesDirectoryExist path
  if fileExists
    then Dir.makeAbsolute path
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      userHome <- Dir.getHomeDirectory
      let absPath = userHome </> "dev/projects/lamdera-compiler" </> path

      exists2 <- doesDirectoryExist absPath
      if exists2
        then pure absPath
        else
          error $ "could not find a relative path, seeking at:\n" <> path <> "\n" <> absPath
