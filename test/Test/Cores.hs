module Test.Cores where

{-| Lamdera.Cores: --cores, LAMDERA_CORES and the cgroup CPU quota. The cgroup reading
runs against fake /proc and /sys trees.
-}

import qualified System.Directory as Dir
import System.FilePath ((</>), takeDirectory)

import EasyTest
import Lamdera (rmdir)

import qualified Lamdera.Cores as Cores


all = EasyTest.run suite


suite :: Test ()
suite =
  tests
    [ scope "the flag wins over the environment" $
        expectEqual (Cores.FromFlag 3) (Cores.choose (Just 3) (Just "5") (Just 1) 12)
    , scope "the environment wins over the quota" $
        expectEqual (Cores.FromEnv 5) (Cores.choose Nothing (Just "5") (Just 1) 12)
    , scope "a bad LAMDERA_CORES is ignored" $
        expectEqual (Cores.FromQuota 2) (Cores.choose Nothing (Just "lots") (Just 2) 12)
    , scope "a quota below the default lowers it" $
        expectEqual (Cores.FromQuota 2) (Cores.choose Nothing Nothing (Just 2) 12)
    , scope "a quota above the default leaves it" $
        expectEqual Cores.Default (Cores.choose Nothing Nothing (Just 32) 12)
    , scope "no quota, the default" $
        expectEqual Cores.Default (Cores.choose Nothing Nothing Nothing 12)
    , scope "cpu.max" $ do
        expectEqual (Just 2.0) (Cores.parseCpuMax "200000 100000\n")
        expectEqual (Just 0.5) (Cores.parseCpuMax "50000 100000")
        expectEqual Nothing (Cores.parseCpuMax "max 100000\n")
    , scope "cfs quota and period" $ do
        expectEqual (Just 1.5) (Cores.parseCfs "150000\n" "100000\n")
        expectEqual Nothing (Cores.parseCfs "-1\n" "100000\n")
    , scope "cgroup v2, quota on the process's cgroup" $ do
        n <- io $ fake
          [ ("proc/self/cgroup", "0::/app.slice/build.scope\n")
          , ("sys/fs/cgroup/cpu.max", "max 100000\n")
          , ("sys/fs/cgroup/app.slice/build.scope/cpu.max", "150000 100000\n")
          ]
        expectEqual (Just 2) n
    , scope "cgroup v2, the smallest quota of the ancestors" $ do
        n <- io $ fake
          [ ("proc/self/cgroup", "0::/a/b\n")
          , ("sys/fs/cgroup/a/cpu.max", "300000 100000\n")
          , ("sys/fs/cgroup/a/b/cpu.max", "max 100000\n")
          ]
        expectEqual (Just 3) n
    , scope "cgroup v2 in a container (cgroup namespace): the root" $ do
        n <- io $ fake
          [ ("proc/self/cgroup", "0::/\n")
          , ("sys/fs/cgroup/cpu.max", "400000 100000\n")
          ]
        expectEqual (Just 4) n
    , scope "cgroup v1" $ do
        n <- io $ fake
          [ ("proc/self/cgroup", "12:memory:/docker/abc\n4:cpu,cpuacct:/docker/abc\n")
          , ("sys/fs/cgroup/cpu,cpuacct/cpu.cfs_quota_us", "-1\n")
          , ("sys/fs/cgroup/cpu,cpuacct/cpu.cfs_period_us", "100000\n")
          , ("sys/fs/cgroup/cpu,cpuacct/docker/abc/cpu.cfs_quota_us", "100000\n")
          , ("sys/fs/cgroup/cpu,cpuacct/docker/abc/cpu.cfs_period_us", "100000\n")
          ]
        expectEqual (Just 1) n
    , scope "no limits" $ do
        n <- io $ fake
          [ ("proc/self/cgroup", "0::/user.slice\n")
          , ("sys/fs/cgroup/user.slice/cpu.max", "max 100000\n")
          ]
        expectEqual Nothing n
    , scope "no cgroups at all" $ do
        n <- io $ fake []
        expectEqual Nothing n
    ]


fake :: [(FilePath, String)] -> IO (Maybe Int)
fake files = do
  root <- Dir.makeAbsolute "./test/scenario-cores"
  rmdir root
  Dir.createDirectoryIfMissing True root
  mapM_ (\(path, text) -> do
          Dir.createDirectoryIfMissing True (takeDirectory (root </> path))
          writeFile (root </> path) text
        ) files
  n <- Cores.cgroupCpusUnder root
  rmdir root
  pure n
