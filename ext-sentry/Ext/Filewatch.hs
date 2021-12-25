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

          shouldRefresh = do
                not (List.isInfixOf ".git" f)
             && not (List.isInfixOf "elm-stuff" f)

        onlyWhen shouldRefresh $ Debounce.send trigger f
      )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

watchFile file action =
  watch (FP.takeDirectory file) action
