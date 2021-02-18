{-# LANGUAGE OverloadedStrings #-}

module Ext.Sentry where

-- import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)

import Control.Concurrent.MVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B


import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

import Lamdera

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


track label io = do
  m <- getTime Monotonic
  p <- getTime ProcessCPUTime
  t <- getTime ThreadCPUTime
  res <- io
  m_ <- getTime Monotonic
  p_ <- getTime ProcessCPUTime
  t_ <- getTime ThreadCPUTime
  fprint ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % "\n") m m_ p p_ t t_
  -- fprint (timeSpecs % "\n") p p_
  -- fprint (timeSpecs % "\n") t t_
  pure res
