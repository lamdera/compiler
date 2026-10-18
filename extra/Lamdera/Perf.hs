{-# LANGUAGE BangPatterns #-}
module Lamdera.Perf
  ( record
  , phase
  , enabled
  )
  where

{-| Opt-in build timing, for finding where compile time goes.

Set LAMDERA_PERF_LOG=/some/file and every recorded step appends one line:

    label <TAB> start ns <TAB> end ns <TAB> bytes allocated <TAB> capability

Start and end are monotonic clock nanoseconds, so lines from different threads can be
laid out on one timeline. Bytes allocated come from the per-thread allocation counter,
which is exact for the recording thread (each module compiles on its own thread), and
includes any thunks it forced that were built elsewhere.

`phase` times a pure Either-producing phase to weak head normal form. That is when the
phase has decided success or failure, which for the compiler's Result-style phases means
it has walked the whole module, but pieces of its output can still be forced later by
whichever phase consumes them. The cost-centre profile (stack build --profile) is the
exact attribution; this is the cheap one that runs on a normal build.

When the variable is unset, both are the identity.
-}

import Control.Concurrent (myThreadId, threadCapability)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import qualified Data.List as List
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (getAllocationCounter)


{-# NOINLINE logHandle #-}
logHandle :: Maybe (MVar Handle)
logHandle =
  unsafePerformIO $
  do  maybePath <- lookupEnv "LAMDERA_PERF_LOG"
      case maybePath of
        Nothing ->
          return Nothing

        Just path ->
          do  h <- openFile path AppendMode
              hSetBuffering h LineBuffering
              Just <$> newMVar h


enabled :: Bool
enabled =
  case logHandle of
    Nothing -> False
    Just _  -> True


record :: String -> IO a -> IO a
record label io =
  case logHandle of
    Nothing ->
      io

    Just mvar ->
      do  t0 <- getMonotonicTimeNSec
          a0 <- getAllocationCounter
          r <- io
          a1 <- getAllocationCounter
          t1 <- getMonotonicTimeNSec
          (cap, _) <- threadCapability =<< myThreadId
          withMVar mvar $ \h ->
            hPutStrLn h $ List.intercalate "\t" [label, show t0, show t1, show (a0 - a1), show cap]
          return r


{-# NOINLINE phase #-}
phase :: String -> Either e a -> Either e a
phase label result =
  case logHandle of
    Nothing ->
      result

    Just _ ->
      unsafePerformIO $ record label (evaluate result)
