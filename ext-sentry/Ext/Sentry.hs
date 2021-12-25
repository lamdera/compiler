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
asyncUpdateJsOutput (Cache mJsOutput) recompile = do
  trackedForkIO "Ext.Sentry.asyncUpdateJsOutput" $ do
    takeMVar mJsOutput
    !bs <- track "recompile" $ recompile
    putMVar mJsOutput bs
    System.Mem.performMajorGC
    pure ()
