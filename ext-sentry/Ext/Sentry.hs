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
    , htmlWrapper :: MVar (BS.ByteString, BS.ByteString)
    }


init :: IO Cache
init = do
  mJsOutput <- newMVar "cacheInit!"
  mHtmlWrapper <- newMVar ("cacheInit!", "cacheInit!")
  pure $ Cache mJsOutput mHtmlWrapper


getJsOutput :: Cache -> IO BS.ByteString
getJsOutput cache =
  readMVar $ jsOutput cache


getHtmlOutput :: Cache -> IO BS.ByteString
getHtmlOutput cache = do
  (htmlPre, htmlPost) <- readMVar $ htmlWrapper cache
  js <- getJsOutput cache
  pure $ htmlPre <> js <> htmlPost


asyncUpdateJsOutput :: Cache -> IO (BS.ByteString, BS.ByteString, BS.ByteString) -> IO ()
asyncUpdateJsOutput (Cache mJsOutput mHtmlWrapper) recompile = do
  trackedForkIO "Ext.Sentry.asyncUpdateJsOutput" $ do
    takeMVar mJsOutput
    takeMVar mHtmlWrapper
    (!pre, !js, !post) <- track "recompile" $ recompile
    putMVar mJsOutput js
    putMVar mHtmlWrapper (pre, post)
    System.Mem.performMajorGC
    pure ()
