{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.Sentry where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified System.Mem

import Lamdera
import Ext.Common


data Cache =
  Cache
    { jsOutput :: MVar BS.ByteString
    }


init :: IO Cache
init = do
  mJsOutput <- newMVar "cacheInit!"
  pure $ Cache mJsOutput


getJsOutput :: Cache -> IO BS.ByteString
getJsOutput cache =
  readMVar $ jsOutput cache


asyncUpdateJsOutput :: Cache -> IO BS.ByteString -> IO ()
asyncUpdateJsOutput cache recompile =
  asyncUpdateJsOutputWith True cache recompile


{-| `lamdera live --memory` passes False: it keeps its artifacts in memory, so the major GC
after each rebuild mostly copies them around, and a Ctrl-C that lands while it runs is
lost (seen on a 920 module app, with ~1GB to copy).
-}
asyncUpdateJsOutputWith :: Bool -> Cache -> IO BS.ByteString -> IO ()
asyncUpdateJsOutputWith majorGC (Cache mJsOutput) recompile = do
  trackedForkIO "Ext.Sentry.asyncUpdateJsOutput" $ do
    takeMVar mJsOutput
    !bs <- track "recompile" $ recompile
    putMVar mJsOutput bs
    if majorGC then System.Mem.performMajorGC else pure ()
    pure ()
