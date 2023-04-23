{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Ext.Common
import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Data.List as List
import qualified Control.FoldDebounce as Debounce
import qualified System.Directory as Dir
import qualified System.FilePath as FP

watch root action =
  trackedForkIO "Ext.Filewatch.watch" $ withManager $ \mgr -> do
    trigger <-
      Debounce.new
        Debounce.Args
          { Debounce.cb = (\events -> action events)
          , Debounce.fold = (\l v -> List.nub $ v:l)
          , Debounce.init = []
          }
        Debounce.def
          { Debounce.delay = 10000 -- 10ms
          , Debounce.alwaysResetTimer = True
          }

    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          f = case e of
                Added f _ _ -> f
                Modified f _ _ -> f
                Removed f _ _ -> f
                Unknown f _ _ -> f

          -- @TODO it would be better to not listen to these folders in the `watchTree` when available
          -- https://github.com/haskell-fswatch/hfsnotify/issues/101
          shouldRefresh = do
                not (List.isInfixOf ".git" f)
             && not (List.isInfixOf "elm-stuff" f)
             && not (List.isInfixOf "node_modules" f)
             && not (List.isInfixOf "data" f)
             && not (List.isInfixOf "elm-pkg-js-includes.min.js" f)

        onlyWhen shouldRefresh $ Debounce.send trigger f
      )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

watchFile file action =
  watch (FP.takeDirectory file) action
