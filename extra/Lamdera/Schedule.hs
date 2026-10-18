{-# LANGUAGE BangPatterns #-}
module Lamdera.Schedule
  ( Scheduler
  , new
  , withSlot
  , priorities
  )
  where

{-| Which module compiles next.

Build forks a thread per module, and every module whose dependencies are done is
runnable at once: on a large app, after a central Types module finishes, ~100 modules
compete for the cores in round robin, and the chain of modules everything else waits
for (the critical path) gets a hundredth of the machine. The build then takes as long
as that chain at that share.

A Scheduler hands out one slot per capability. A module takes a slot to compile (after
its dependencies are done, so a module holding a slot never waits on another), and
when slots run out, the waiting module with the highest priority goes next. Priority
is the length of the longest chain of modules that depend on it, weighted by source
size, so the critical path is never starved.

-}

import Control.Concurrent.MVar
import Control.Exception (mask, onException)
import qualified Data.Map as MapLazy
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))


data Scheduler k =
  Scheduler
    { _state :: MVar State
    , _priority :: Map.Map k Int
    }


data State =
  State
    { _free :: !Int
    , _next :: !Int
    , _waiting :: !(Map.Map (Down Int, Int) (MVar ()))
    }


new :: Int -> Map.Map k Int -> IO (Scheduler k)
new slots priority =
  do  state <- newMVar (State (max 1 slots) 0 Map.empty)
      return (Scheduler state priority)


withSlot :: Ord k => Scheduler k -> k -> IO a -> IO a
withSlot (Scheduler stateMVar priority) key work =
  mask $ \restore ->
    do  acquire stateMVar (Map.findWithDefault 0 key priority)
        result <- restore work `onException` release stateMVar
        release stateMVar
        return result


acquire :: MVar State -> Int -> IO ()
acquire stateMVar prio =
  do  maybeWait <-
        modifyMVar stateMVar $ \(State free next waiting) ->
          if free > 0 then
            return (State (free - 1) next waiting, Nothing)
          else
            do  wake <- newEmptyMVar
                return (State free (next + 1) (Map.insert (Down prio, next) wake waiting), Just wake)
      case maybeWait of
        Nothing -> return ()
        Just wake -> takeMVar wake


-- Hand the slot to the waiting module with the highest priority (first come first
-- served among equals), or free it.
release :: MVar State -> IO ()
release stateMVar =
  modifyMVar_ stateMVar $ \(State free next waiting) ->
    case Map.minViewWithKey waiting of
      Just ((_, wake), rest) ->
        do  putMVar wake ()
            return (State free next rest)

      Nothing ->
        return (State (free + 1) next waiting)


{-| For each module: its own cost plus the costliest chain of modules that depend on
it, given each module's dependencies and cost. Dependencies outside the map are
ignored. The graph must have no cycles.
-}
priorities :: Ord k => Map.Map k [k] -> Map.Map k Int -> Map.Map k Int
priorities deps cost =
  let
    dependents =
      Map.fromListWith (++)
        [ (d, [m]) | (m, ds) <- Map.toList deps, d <- ds, Map.member d deps ]

    -- lazily memoised over the map. Only built once Build has checked there are no
    -- import cycles (checkMidpoint), so this terminates.
    heights =
      MapLazy.mapWithKey height deps

    height m _ =
      let !own = Map.findWithDefault 1 m cost
          above = maximum (0 : [ Map.findWithDefault 0 d heights | d <- Map.findWithDefault [] m dependents ])
      in own + above
  in
  heights
