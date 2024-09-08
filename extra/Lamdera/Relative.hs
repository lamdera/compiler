module Lamdera.Relative where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Data.List as List

import Lamdera

-- These help with loading relative file paths from the compiler codebase when we're developing in GHCI and the current working directory isn't reliably set
-- This happens because the Elm compiler relies on the current working directory to find project roots, so in our test suite we end up needing to modify this value a fair bit
-- These helpers currently expect that the lamdera-compiler codebase is checked out to `~/dev/projects/lamdera-compiler`

expectedCompilerPath :: IO FilePath
expectedCompilerPath = do
  userHome <- Dir.getHomeDirectory
  pure $ userHome </> "dev/projects/lamdera-compiler"


prefixCompilerPath :: String -> IO FilePath
prefixCompilerPath path = do
  compilerPath <- expectedCompilerPath
  pure $ compilerPath </> path


findFile :: String -> IO (Maybe FilePath)
findFile path_ = do
  path <- resolveHome path_
  fileExists <- doesFileExist path
  if fileExists
    then pure (Just path)
    else do
      absPath <- prefixCompilerPath path
      exists2 <- doesFileExist absPath
      if exists2
        then pure (Just absPath)
        else do
          debug $ "findFile: could not find a relative path, sought at:\n" <> path_ <> " -> " <> path <> "\n" <> absPath
          pure Nothing


readFile :: String -> IO (Maybe Text)
readFile path = do
  found <- findFile path
  case found of
    Just absPath -> Lamdera.readUtf8Text absPath
    Nothing -> pure Nothing


readByteString :: FilePath -> IO BS.ByteString
readByteString path = do
  fullPath <- requireFile path
  BS.readFile fullPath


writeFile :: FilePath -> Text -> IO ()
writeFile path content = do
  found <- findFile path
  case found of
    Just absPath -> Lamdera.writeUtf8 absPath content
    Nothing -> error $ "writeFile: could not find a relative path, seeking at:\n" <> path


requireFile :: String -> IO FilePath
requireFile path = do
  found <- findFile path
  case found of
    Just absPath -> pure absPath
    Nothing -> error $ "requireFile: could not find a relative path, seeking at:\n" <> path


requireDir :: String -> IO FilePath
requireDir path_ = do
  path <- resolveHome path_
  dirExists <- doesDirectoryExist path
  if dirExists
    then Dir.makeAbsolute path
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      absPath <- prefixCompilerPath path
      exists2 <- doesDirectoryExist absPath
      if exists2
        then pure absPath
        else error $ "requireDir: could not find a relative path, seeking at:\n" <> path <> "\n" <> absPath


resolveHome :: String -> IO FilePath
resolveHome path = do
  if "~" `List.isPrefixOf` path
    then do
      userHome <- Dir.getHomeDirectory
      debug $ "resolveHome: resolved " <> path <> " to " <> userHome </> drop 1 path
      pure $ userHome <> (drop 1 path)
    else do
      debug $ "resolveHome: fallback to original path " <> path
      pure path
