{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Ext.Common
import qualified System.FSNotify as FSNotify
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (forever)
import qualified Data.List as List
import qualified Control.FoldDebounce as Debounce
import qualified System.Directory as Dir
import qualified System.FilePath as FP


{-| The name of the optional per-project file listing extra watcher ignores. -}
ignoreFilename :: FilePath
ignoreFilename =
  ".lamdera-ignore"


{-| Load extra ignore patterns from an optional `.lamdera-ignore` at the watch
root. One pattern per line; blank lines and `#` comments are skipped. Patterns
are matched as a substring of the changed path, exactly like the built-in
ignores in `watch`, so a line such as `.playwright-mcp` ignores everything
under any directory of that name.

Read once at watch boot, so edits to the file take effect on next start.
-}
loadIgnorePatterns :: FilePath -> IO [String]
loadIgnorePatterns root = do
  let path = root FP.</> ignoreFilename
  exists <- Dir.doesFileExist path
  if not exists
    then pure []
    else do
      contents <- readFile path
      -- `readFile` is lazy; force it so the handle closes before we return.
      _ <- evaluate (length contents)
      pure
        $ filter (\l -> not (null l) && not (List.isPrefixOf "#" l))
        $ fmap trim
        $ lines contents


{-| Should a change to this path trigger a refresh? Paths that are never a
project's own source are always skipped, plus any extra patterns loaded from
`.lamdera-ignore` (see `loadIgnorePatterns`).

@TODO it would be better to not listen to these folders in the `watchTree` when
available: https://github.com/haskell-fswatch/hfsnotify/issues/101
-}
shouldRefreshPath :: [String] -> FilePath -> Bool
shouldRefreshPath userIgnores filepath =
      not (List.isInfixOf ".git" filepath)
   && not (List.isInfixOf ".jj" filepath)
   && not (List.isInfixOf ".watchman-cookie-" filepath)
   && not (List.isInfixOf "elm-stuff" filepath)
   && not (List.isInfixOf "node_modules" filepath)
  --  This is really dumb of you because some people use `/data/...` as a folder...
  --  && not (List.isInfixOf "data" filepath)
   && not (List.isInfixOf "elm-pkg-js-includes.min.js" filepath)
   && not (List.isInfixOf "tests" filepath)
   && not (any (\p -> List.isInfixOf p filepath) userIgnores)


watch :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watch root action = do
  let config = FSNotify.defaultConfig { FSNotify.confOnHandlerException = \e -> Ext.Common.debug ("fsnotify: handler threw exception: " <> show e) }

  userIgnores <- loadIgnorePatterns root

  trackedForkIO "Ext.Filewatch.watch" $ FSNotify.withManagerConf config $ \mgr -> do
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
    onlyWhen (not (null userIgnores)) $
      Ext.Common.debug $ "👀 " ++ ignoreFilename ++ " patterns: " ++ show userIgnores
    -- start a watching job (in the background)
    _ <- FSNotify.watchTree
      mgr          -- manager
      root         -- directory to watch
      (const True) -- predicate
      (\e -> do
        let
          filepath = case e of
            FSNotify.Added f _ _ -> f
            FSNotify.Modified f _ _ -> f
            FSNotify.ModifiedAttributes f _ _ -> f
            FSNotify.Removed f _ _ -> f
            FSNotify.WatchedDirectoryRemoved f _ _ -> f
            FSNotify.CloseWrite f _ _ -> f
            FSNotify.Unknown f _ _ _ -> f

          shouldRefresh = shouldRefreshPath userIgnores filepath

          indicator = if shouldRefresh then "👀 file event" else "🙈 ignoring  "

        Ext.Common.debug $ indicator ++ " " ++ show e
        onlyWhen shouldRefresh $ Debounce.send trigger filepath
      )

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000


watchFile :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watchFile file action =
  watch (FP.takeDirectory file) action
