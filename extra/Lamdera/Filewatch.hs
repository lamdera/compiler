{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Filewatch where

import Lamdera
import SocketServer
import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Data.List as List
import Control.Debounce

watch (mClients, mLeader, mChan, beState) =
  forkIO $ withManager $ \mgr -> do

    broadcastRefresh <- mkDebounce defaultDebounceSettings
      { debounceAction = do
          Lamdera.debug "reloading all clients, starting 500ms reload cooldown"
          SocketServer.broadcastImpl mClients "{\"t\":\"r\"}" -- r is refresh, see live.js
      , debounceFreq = 500000 -- 500 milliseconds
      , debounceEdge = leadingEdge -- Trigger on the trailing edge
      }

    root <- getProjectRoot

    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          shouldRefresh = do
            let
              check f =
                   not (List.isInfixOf ".git" f)
                && not (List.isInfixOf "elm-stuff" f)
                && not (List.isInfixOf "lamdera-stuff" f)
            check $ case e of
              Added f _ _ -> f
              Modified f _ _ -> f
              Removed f _ _ -> f
              Unknown f _ _ -> f

        if shouldRefresh
          then do
            Lamdera.debug $ "change trigger: " <> show e
            broadcastRefresh
          else
            pure ()
      )  -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
