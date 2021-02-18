module Ext.Common where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified System.Environment as Env
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)


{- Concurrent debugging
-}


debug_ :: String -> IO ()
debug_ str = do
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
      debug_ $ "Tracking GHCI thread:" ++ show threadId
      pure $ threadId:threads
    )


killTrackedThreads :: IO ()
killTrackedThreads = do
  modifyMVar_ ghciThreads
    (\threads -> do
      case threads of
        [] -> do
          debug_ $ "No tracked GHCI threads to kill."
          pure []
        threads -> do
          debug_ $ "Killing tracked GHCI threads: " ++ show threads
          mapM killThread threads
          pure []
    )


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE ghciThreads #-}
ghciThreads :: MVar [ThreadId]
ghciThreads = unsafePerformIO $ newMVar []
