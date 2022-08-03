module Lamdera.RelativeLoad where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import Lamdera

find :: FilePath -> IO BS.ByteString
find path = do
  let
    liveRaw = "extra/live.js"
    liveBuilt = "extra/dist/live.js"
  exists <- doesFileExist path
  if exists
    then BS.readFile path
    else do
      -- We're likely using a GHCI build mode that's changed our currentDirectory, so now Haskell is confused.
      -- Only thing we can really do now is guess from a standard-ish location relative to home
      userHome <- Dir.getHomeDirectory
      let relativePath = userHome </> "dev/projects/lamdera-compiler" </> path

      exists2 <- doesFileExist relativePath
      if exists2
        then BS.readFile relativePath
        else
          error $ "could not find any of the live build paths:\n" <> path <> "\n" <> relativePath
