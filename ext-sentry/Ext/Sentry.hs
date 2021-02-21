{-# LANGUAGE OverloadedStrings #-}

module Ext.Sentry where

-- import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)

import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B

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


updateJsOutput :: Cache -> IO BS.ByteString -> IO ()
updateJsOutput (Cache mJsOutput) recompile = do
  -- debug_ "recompileLocalDev"
  track "recompile" $
    modifyMVar_ mJsOutput
      (\_ -> do
        bs <- recompile
        pure bs
      )

  -- debug_ $ "updateJsOutput: " <> show duration
  -- pure (Cache mJsOutput)
  pure ()
