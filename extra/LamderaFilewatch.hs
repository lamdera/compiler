{-# LANGUAGE OverloadedStrings #-}

module LamderaFilewatch where

import Lamdera
import SocketServer
import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Data.List as List
import Control.Debounce

watch mClients =
  forkIO $ withManager $ \mgr -> do

    broadcastRefresh <- mkDebounce defaultDebounceSettings
      { debounceAction = do
          Lamdera.debug "reloading all clients, starting 500ms reload cooldown"
          SocketServer.broadcastImpl mClients "r" -- r is refresh, see live.js
      , debounceFreq = 500000 -- 500 milliseconds
      , debounceEdge = leadingEdge -- Trigger on the trailing edge
      }

    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          shouldRefresh =
            case e of
              Added filename _ _ ->
                not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)

              Modified filename _ _ ->
                not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)

              Removed filename _ _ ->
                not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)

              Unknown filename _ _ ->
                not (List.isInfixOf "/.git/" filename) && not (List.isInfixOf "/lamdera-stuff/" filename)


        if shouldRefresh
          then do
            Lamdera.debug $ "change trigger: " <> show e
            broadcastRefresh
          else
            pure ()
      )  -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
