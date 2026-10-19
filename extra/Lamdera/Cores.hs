{-# LANGUAGE ScopedTypeVariables #-}
module Lamdera.Cores
  ( withCores
  , apply
  , Choice(..)
  , choose
  , cgroupCpus
  , cgroupCpusUnder
  , parseCpuMax
  , parseCfs
  , helpText
  )
  where

{- @LAMDERA how many cores a compile uses.

The binary starts with `-maxN12` (elm.cabal): one capability per core, at most 12. On
top of that, for the commands that compile (`withCores`):

  1. `--cores N` on the command line,
  2. else LAMDERA_CORES=N in the environment,
  3. else the default: what the RTS started with, lowered to the CPU quota of the cgroup
     the process runs in when that is smaller (Linux containers: cgroup v2 `cpu.max`,
     cgroup v1 `cpu.cfs_quota_us` / `cpu.cfs_period_us`, rounded up). The RTS counts the
     cores it may run on (sched_getaffinity), not a quota, so a container limited to 2
     CPUs on a 64 core host would otherwise run 12 capabilities, each with its own 128MB
     nursery, and spend its quota switching between them.

A number given (1 or 2) is used as it is, above 12 too. `+RTS -N` still sets what the
RTS starts with, so it only matters when neither is given (and the cgroup quota is then
still applied on top). A `--cores` value is also put in LAMDERA_CORES, so lamdera
processes started by this one (e.g. by `lamdera check`) use the same.
-}

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Exception (SomeException, try)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.Info
import Text.Read (readMaybe)

import qualified Terminal.Internal as T
import qualified Lamdera



-- THE FLAG


helpText :: String
helpText =
  "How many cores to compile with. The default is every core up to 12, or fewer when a\
  \ Linux container's CPU quota (cgroup cpu.max, or cfs_quota_us/cfs_period_us) allows\
  \ fewer. LAMDERA_CORES=N in the environment does the same as --cores=N; --cores wins\
  \ when both are given."


{-| Add `--cores N` to a command, and apply it (see `apply`) before the command runs. -}
withCores :: T.Command -> T.Command
withCores (T.Command name summary details example args flags run) =
  T.Command name summary details example args
    (T.FMore (mapFlags (,) flags) (T.Flag "cores" cores helpText))
    (\a (f, n) -> apply n >> run a f)


mapFlags :: (a -> b) -> T.Flags a -> T.Flags b
mapFlags f flags =
  case flags of
    T.FDone a -> T.FDone (f a)
    T.FMore more flag -> T.FMore (mapFlags (f .) more) flag


cores :: T.Parser Int
cores =
  T.Parser
    { T._singular = "number of cores"
    , T._plural = "numbers of cores"
    , T._parser = positive
    , T._suggest = \_ -> return []
    , T._examples = \_ -> return ["1", "4", "8"]
    }


positive :: String -> Maybe Int
positive s =
  case readMaybe s of
    Just n | n >= 1 && n <= 1024 -> Just n
    _ -> Nothing



-- APPLY


data Choice
  = FromFlag Int
  | FromEnv Int
  | FromQuota Int -- the default, lowered to the cgroup quota
  | Default
  deriving (Eq, Show)


{-| What to use, from the flag, LAMDERA_CORES, the cgroup quota (in CPUs) and the
capabilities the RTS started with.
-}
choose :: Maybe Int -> Maybe String -> Maybe Int -> Int -> Choice
choose flag env quota started =
  case (flag, env >>= positive) of
    (Just n, _) -> FromFlag n
    (Nothing, Just n) -> FromEnv n
    _ ->
      case quota of
        Just q | q < started -> FromQuota (max 1 q)
        _ -> Default


apply :: Maybe Int -> IO ()
apply flag =
  do  env <- Env.lookupEnv "LAMDERA_CORES"
      started <- getNumCapabilities
      quota <- if flag == Nothing && (env >>= positive) == Nothing then cgroupCpus else return Nothing
      let choice = choose flag env quota started
      case choice of
        FromFlag n -> Env.setEnv "LAMDERA_CORES" (show n) >> set started n
        FromEnv n -> set started n
        FromQuota n -> set started n
        Default -> return ()
      now <- getNumCapabilities
      Lamdera.debug_ $ "cores: " ++ show choice ++ ", started with " ++ show started ++ ", using " ++ show now


set :: Int -> Int -> IO ()
set started n =
  if n == started then return () else setNumCapabilities n



-- CGROUPS


{-| The CPU quota of the cgroup this process is in, in whole CPUs (rounded up), when
there is one. Linux only.
-}
cgroupCpus :: IO (Maybe Int)
cgroupCpus =
  if System.Info.os /= "linux"
    then return Nothing
    else cgroupCpusUnder "/"


{-| cgroupCpus, with /proc and /sys found under the given directory (for tests). The
smallest quota of the process's cgroup and its ancestors counts, and the cgroup root of
each hierarchy too (in a container the process's cgroup path often is not visible under
/sys/fs/cgroup, which is the container's own cgroup).
-}
cgroupCpusUnder :: FilePath -> IO (Maybe Int)
cgroupCpusUnder root =
  do  self <- readMaybeFile (root </> "proc/self/cgroup")
      let entries = maybe [] (Maybe.mapMaybe parseCgroupLine . lines) self
      v2 <- traverse (\path -> readQuotas v2Quota (root </> "sys/fs/cgroup") path) [ p | (_, ctrls, p) <- entries, null ctrls ]
      let v1Dirs = [ "cpu", "cpu,cpuacct", "cpuacct,cpu" ]
      v1 <- traverse (\(dir, path) -> readQuotas v1Quota (root </> "sys/fs/cgroup" </> dir) path)
              [ (dir, p) | (_, ctrls, p) <- entries, "cpu" `elem` ctrls, dir <- v1Dirs ]
      -- no /proc/self/cgroup (or nothing in it): look at the roots
      fallback <-
        if null entries
          then (:) <$> readQuotas v2Quota (root </> "sys/fs/cgroup") "/" <*> traverse (\dir -> readQuotas v1Quota (root </> "sys/fs/cgroup" </> dir) "/") v1Dirs
          else return []
      let quotas = concat (v2 ++ v1 ++ fallback)
      return $
        case quotas of
          [] -> Nothing
          _ -> Just (max 1 (ceiling (minimum quotas)))


-- "hierarchy-id:controllers:path"; cgroup v2 has no controllers
parseCgroupLine :: String -> Maybe (String, [String], FilePath)
parseCgroupLine line =
  case break (== ':') line of
    (hid, ':' : rest) ->
      case break (== ':') rest of
        (ctrls, ':' : path) -> Just (hid, filter (not . null) (splitOn ',' ctrls), path)
        _ -> Nothing
    _ -> Nothing


splitOn :: Char -> String -> [String]
splitOn c s =
  case break (== c) s of
    (a, _ : rest) -> a : splitOn c rest
    (a, []) -> [a]


-- The quotas in the cgroup at `path` under a hierarchy's mount, and in each ancestor.
readQuotas :: (FilePath -> IO (Maybe Double)) -> FilePath -> FilePath -> IO [Double]
readQuotas quotaIn mount path =
  do  let parts = filter (not . null) (splitOn '/' path)
          dirs = List.nub (mount : [ foldl (</>) mount (take n parts) | n <- [1 .. length parts] ])
      Maybe.catMaybes <$> traverse quotaIn dirs


v2Quota :: FilePath -> IO (Maybe Double)
v2Quota dir =
  (>>= parseCpuMax) <$> readMaybeFile (dir </> "cpu.max")


v1Quota :: FilePath -> IO (Maybe Double)
v1Quota dir =
  do  quota <- readMaybeFile (dir </> "cpu.cfs_quota_us")
      period <- readMaybeFile (dir </> "cpu.cfs_period_us")
      return $
        case (quota, period) of
          (Just q, Just p) -> parseCfs q p
          _ -> Nothing


{-| cgroup v2 cpu.max: "max 100000" (no limit) or "QUOTA PERIOD", in CPUs. -}
parseCpuMax :: String -> Maybe Double
parseCpuMax text =
  case words text of
    [quota, period] -> ratio quota period
    [quota] -> ratio quota "100000"
    _ -> Nothing


{-| cgroup v1 cfs_quota_us (-1 for no limit) and cfs_period_us, in CPUs. -}
parseCfs :: String -> String -> Maybe Double
parseCfs quota period =
  ratio (trim quota) (trim period)


ratio :: String -> String -> Maybe Double
ratio quota period =
  case (readMaybe quota :: Maybe Integer, readMaybe period :: Maybe Integer) of
    (Just q, Just p) | q > 0 && p > 0 -> Just (fromIntegral q / fromIntegral p)
    _ -> Nothing


trim :: String -> String
trim =
  List.dropWhileEnd (`elem` " \n\t") . dropWhile (`elem` " \n\t")


readMaybeFile :: FilePath -> IO (Maybe String)
readMaybeFile path =
  do  exists <- Dir.doesFileExist path
      if not exists
        then return Nothing
        else
          do  result <- try (readFile path >>= \s -> length s `seq` return s)
              return $
                case result of
                  Right s -> Just s
                  Left (_ :: SomeException) -> Nothing
