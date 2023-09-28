{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Ext.Common
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.List as List
import qualified Control.FoldDebounce as Debounce
import qualified System.FilePath as FP


watch :: FilePath -> ([FilePath] -> IO ()) -> IO ()
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

    Ext.Common.debug $ "👀 file watch booting for " ++ show root
    -- start a watching job (in the background)
    _ <- watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          filepath = case e of
            Added f _ _ -> f
            Modified f _ _ -> f
            ModifiedAttributes f _ _ -> f
            Removed f _ _ -> f
            WatchedDirectoryRemoved f _ _ -> f
            CloseWrite f _ _ -> f
            Unknown f _ _ _ -> f

          -- @TODO it would be better to not listen to these folders in the `watchTree` when available
          -- https://github.com/haskell-fswatch/hfsnotify/issues/101
          shouldRefresh = do
                not (List.isInfixOf ".git" filepath)
             && not (List.isInfixOf "elm-stuff" filepath)
             && not (List.isInfixOf "node_modules" filepath)
             && not (List.isInfixOf "data" filepath)
             && not (List.isInfixOf "elm-pkg-js-includes.min.js" filepath)

        Ext.Common.debug $ "👀 file event " ++ show e ++ " with shouldRefresh:" ++ show shouldRefresh
        onlyWhen shouldRefresh $ Debounce.send trigger filepath
      )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000


watchFile :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watchFile file action =
  watch (FP.takeDirectory file) action
