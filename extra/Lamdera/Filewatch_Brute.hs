{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Filewatch_Brute where

-- Because of https://gitlab.haskell.org/ghc/ghc/-/issues/18446
-- We cannot build https://github.com/luite/hfsevents/blob/master/hfsevents.cabal#L24
-- So fsnotify fails to build on macOS Big Sur.
--
-- This is a temporary poor-mans brute force version of the same API as Filewatch.hs,
-- used only on macOS.

import SocketServer
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Control.Concurrent.MVar (newMVar, modifyMVar_, MVar)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.Directory.Tree
import qualified Data.List as List
import Data.Time.Clock (UTCTime, getCurrentTime)

import Lamdera


watch (mClients, mLeader, mChan, beState) = do
  root <- getProjectRoot
  forkIO $
    forever $ do
      checkForChanges root $ do
        Lamdera.debug "reloading all clients, starting 500ms reload cooldown"
        SocketServer.broadcastImpl mClients "{\"t\":\"r\"}" -- r is refresh, see live.js

      threadDelay 1000000



getDirTree :: FilePath -> IO (DirTree (FilePath, UTCTime))
getDirTree root = do
  aDirTree <-
    readDirectoryWith
      (\f -> do
        t <- getModificationTime f
        pure (f, t)
      )
      root
  pure $ removeIgnoredDirectories aDirTree


removeIgnoredDirectories :: AnchoredDirTree (FilePath, UTCTime) -> DirTree (FilePath, UTCTime)
removeIgnoredDirectories ((:/) anchor dirTree) =
  dirTree
    & filterDir (\d ->
      case d of
        Failed name err -> False
        Dir name contents -> do
              not (List.isInfixOf ".git" name)
           && not (List.isInfixOf "elm-stuff" name)
           && not (List.isInfixOf "lamdera-stuff" name) -- @LEGACY
        File name file -> True
    )


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE lastDirTree #-}
lastDirTree :: MVar (DirTree (FilePath, UTCTime))
lastDirTree = unsafePerformIO $ do
  ctime <- getCurrentTime
  newMVar (File "init" ("init", ctime))


checkForChanges :: FilePath -> IO () -> IO ()
checkForChanges root callback =
  modifyMVar_ lastDirTree
    (\lastTree -> do
      nowTree <- getDirTree root
      onlyWhen (lastTree /= nowTree) callback
      -- if (lastTree == nowTree)
      --   then do
      --     putStrLn "no changes!"
      --     putStrLn $ show nowTree
      --   else do
      --     putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
      --     putStrLn $ show nowTree
      --     callback
      pure nowTree
    )


test = do
  root <- getProjectRoot
  res1 <- getDirTree root
  res2 <- getDirTree root
  case res1 == res2 of
    True -> putStrLn "test1 equal"
    False -> putStrLn "test1 diff"

  res3 <- getDirTree root
  touch "/Users/mario/dev/projects/lamdera-test/testing"
  res4 <- getDirTree root
  case res3 == res4 of
    True -> putStrLn "test2 equal"
    False -> putStrLn "test2 diff"

  removeFile "/Users/mario/dev/projects/lamdera-test/testing"
