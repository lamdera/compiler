{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Compile where

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Make (Flags(..))
import qualified Make
import Control.Concurrent.Async
import qualified Data.Text as T

import Lamdera
import qualified Ext.Common


-- Runs `lamdera make --optimize` of given path with no output
makeOptimized :: FilePath -> FilePath -> IO ()
makeOptimized root path = do
  makeOptimizedWithCleanup (pure ()) root path


-- Runs `lamdera make --optimize` of given files with no output, followed by the cleanup IO
makeOptimizedWithCleanup :: IO () -> FilePath -> FilePath -> IO ()
makeOptimizedWithCleanup cleanup root path = do
  debug $ "🏗   makeOptimizedWithCleanup: lamdera make --optimize " <> root <> "/" <> path

  absRoot <- Dir.makeAbsolute root

  let
    tmp = lamderaCache absRoot <> "/tmp.js"
    scaffold = lamderaCache absRoot <> "/Main_.elm"

  writeUtf8 scaffold $ "module Main_ exposing (..)\n\nimport " <> (T.pack $ FP.takeFileName $ FP.dropExtensions path) <> "\nimport Html\n\nmain = Html.text \"\""

  r <- async $
    Ext.Common.withProjectRoot absRoot $
      Make.run_cleanup cleanup [scaffold] $
        Make.Flags
          { _debug = False
          , _optimize = True
          -- We don't use Make.DevNull as it does not actually compile to JS,
          -- thus we never get warnings about Debug.* usage which we want.
          , _output = Just (Make.JS tmp)
          , _report = Nothing
          , _docs = Nothing
          , _noWire = False
          , _optimizeLegible = False
          , _esm = False
          }
  wait r
  remove tmp
  remove scaffold
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


-- Runs `lamdera make` with no JS output
make_ :: FilePath -> IO ()
make_ root = do
  debug $ "🏗   make_: lamdera make " <> root <> "/"

  r <- async $
    Ext.Common.withProjectRoot root $
      Make.run [] $
        Make.Flags
          { _debug = False
          , _optimize = True
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          , _noWire = False
          , _optimizeLegible = True
          , _esm = False
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10



-- Runs `lamdera make` of given files with no JS file output
makeDev :: FilePath -> [FilePath] -> IO ()
makeDev root paths = do
  debug $ "🏗   makeDev: lamdera make " <> root <> "/" <> show paths

  absRoot <- Dir.makeAbsolute root

  r <- async $
    Ext.Common.withProjectRoot absRoot $ do
      mapM touch paths
      Make.run paths $
        Make.Flags
          { _debug = True
          , _optimize = False
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          , _noWire = False
          , _optimizeLegible = False
          , _esm = False
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


makeDev_ :: FilePath -> IO ()
makeDev_ path =
  makeDev (FP.takeDirectory path) [path]


makeDevHtml :: FilePath -> [FilePath] -> IO ()
makeDevHtml root paths = do
  debug $ "🏗   makeDevHtml: lamdera make " <> root <> "/"

  r <- async $
    Ext.Common.withProjectRoot root $ do
      Make.run paths $
        Make.Flags
          { _debug = True
          , _optimize = False
          , _output = Nothing
          , _report = Nothing
          , _docs = Nothing
          , _noWire = False
          , _optimizeLegible = False
          , _esm = False
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


-- Runs `lamdera make` of harness file with JS file output
makeHarnessDevJs :: FilePath -> IO ()
makeHarnessDevJs root = do
  let
    tmp = lamderaCache root <> "/tmp.js"
    scaffold = lamderaCache root <> "/Main_.elm"

  debug $ "🏗   makeHarnessDevJs: lamdera make " <> scaffold

  writeUtf8 scaffold "module Main_ exposing (..)\n\nimport Frontend\nimport Backend\nimport Types\nimport Html\n\nmain = Html.text \"\""

  r <- async $
    Ext.Common.withProjectRoot root $
      Make.run [scaffold] $
        Make.Flags
          { _debug = True
          , _optimize = False
          , _output = Just (Make.JS tmp)
          , _report = Nothing
          , _docs = Nothing
          , _noWire = False
          , _optimizeLegible = False
          , _esm = False
          }
  wait r
  remove tmp
  remove scaffold

  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10
