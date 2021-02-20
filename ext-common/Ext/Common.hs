module Ext.Common where

import Control.Concurrent
import Control.Concurrent.MVar

import System.Exit (exitFailure)
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)
import qualified System.Directory as Dir
import qualified System.Environment as Env

-- Re-exports
import qualified Data.Function


-- Copy of combined internals of Project.getRoot as it seems to notoriously cause cyclic wherever imported
getProjectRoot :: IO FilePath
getProjectRoot = do
  subDir <- Dir.getCurrentDirectory
  res <- findHelp "elm.json" (FP.splitDirectories subDir)
  case res of
    Just filepath -> pure filepath
    Nothing -> do
      binName <- Env.getProgName
      putStrLn $ "Cannot find an elm.json! Make sure you're in a project folder, or run `" <> binName <> " init` to start a new one."
      debug $ "current directory was: " <> subDir
      exitFailure


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if Prelude.null dirs then
    return Nothing

  else
    do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists_
          then return (Just (FP.joinPath dirs))
          else findHelp name (Prelude.init dirs)


-- Find the project root from an arbitrary fle path
getProjectRootFor :: FilePath -> IO FilePath
getProjectRootFor path = do
  res <- findHelp "elm.json" (FP.splitDirectories $ takeDirectory path)
  case res of
    Just filepath -> pure filepath
    Nothing -> do
      binName <- Env.getProgName
      putStrLn $ "Cannot find an elm.json! Make sure you're in a project folder, or run `" <> binName <> " init` to start a new one."
      exitFailure


{- Helpers -}

justs :: [Maybe a] -> [a]
justs xs = [ x | Just x <- xs ]




{- Concurrent debugging
-}


debug :: String -> IO ()
debug str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str ++ "\n"
    Nothing -> pure ()


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = unsafePerformIO $ newMVar ()


{- Print debugging in a concurrent setting can be painful sometimes due to output
becoming interpolated. This `putStrLn` alternative uses an MVar to ensure all
printouts are atomic and un-garbled.
-}
atomicPutStrLn :: String -> IO ()
atomicPutStrLn str =
  withMVar printLock (\_ -> hPutStr stdout (str <> "\n") >> hFlush stdout)



{- GHCI thread management

Developing threaded processes in GHCI can be rather tricky, as threads are directly
invoked from the main GHCI thread, so they don't die unless you kill GHCI and reload,
a rather slow process.

In order to get closer to the holy grail of "":r + kill + reload threads", useful
when working on and testing a daemon, the `trackedForkIO` function is a drop-in
replacement for `forkIO`, which paired with `killTrackedThreads` lets us cleanup
after a `:r` and avoid issues like a socket port already being in use!

-}


trackedForkIO :: IO () -> IO ()
trackedForkIO io = do
  threadId <- forkIO io
  trackGhciThread threadId


trackGhciThread :: ThreadId -> IO ()
trackGhciThread threadId =
  modifyMVar_ ghciThreads
    (\threads -> do
      debug $ "Tracking GHCI thread:" ++ show threadId
      pure $ threadId:threads
    )


killTrackedThreads :: IO ()
killTrackedThreads = do
  modifyMVar_ ghciThreads
    (\threads -> do
      case threads of
        [] -> do
          debug $ "No tracked GHCI threads to kill."
          pure []
        threads -> do
          debug $ "Killing tracked GHCI threads: " ++ show threads
          mapM killThread threads
          pure []
    )


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE ghciThreads #-}
ghciThreads :: MVar [ThreadId]
ghciThreads = unsafePerformIO $ newMVar []



-- Re-exports

(&) = (Data.Function.&)
