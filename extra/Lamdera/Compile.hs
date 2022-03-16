{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Compile where

import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Make (Flags(..))
import qualified Make
import Control.Concurrent.Async
import qualified Data.Text as T

import Lamdera


-- Runs `lamdera make --optimize` of given path with no output
makeOptimized :: FilePath -> FilePath -> IO ()
makeOptimized root path = do
  makeOptimizedWithCleanup (pure ()) root path


-- Runs `lamdera make --optimize` of given files with no output, followed by the cleanup IO
makeOptimizedWithCleanup :: IO () -> FilePath -> FilePath -> IO ()
makeOptimizedWithCleanup cleanup root path = do
  debug $ "🏗   lamdera make --optimize " <> root <> "/" <> path
  let
    tmp = lamderaCache root <> "/tmp.js"
    scaffold = lamderaCache root <> "/Main_.elm"

  writeUtf8 scaffold $ "module Main_ exposing (..)\n\nimport " <> (T.pack $ FP.takeFileName $ FP.dropExtensions path) <> "\nimport Html\n\nmain = Html.text \"\""

  r <- async $
    Dir.withCurrentDirectory root $
      Make.run_cleanup cleanup [scaffold] $
        Make.Flags
          { _debug = False
          , _optimize = True
          -- We don't use Make.DevNull as it does not actually compile to JS,
          -- thus we never get warnings about Debug.* usage which we want.
          , _output = Just (Make.JS tmp)
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  remove tmp
  remove scaffold
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


-- Runs `lamdera make` with no output
make_ :: FilePath -> IO ()
make_ root = do
  debug $ "🏗   lamdera make " <> root <> "/"

  r <- async $
    Dir.withCurrentDirectory root $
      Make.run [] $
        Make.Flags
          { _debug = False
          , _optimize = True
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10



-- Runs `lamdera make` of given files with no output
makeDev :: FilePath -> FilePath -> IO ()
makeDev root path = do
  debug $ "🏗   lamdera make " <> root <> "/" <> path

  absRoot <- Dir.makeAbsolute root

  r <- async $
    Dir.withCurrentDirectory absRoot $ do
      touch path
      Make.run [path] $
        Make.Flags
          { _debug = True
          , _optimize = False
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


makeDev_ :: FilePath -> IO ()
makeDev_ path =
  makeDev (FP.takeDirectory path) path


-- Runs `lamdera make` of harness file with JS output
makeHarnessDevJs :: FilePath -> IO ()
makeHarnessDevJs root = do
  let
    tmp = lamderaCache root <> "/tmp.js"
    scaffold = lamderaCache root <> "/Main_.elm"

  debug $ "🏗   lamdera make " <> scaffold

  writeUtf8 scaffold "module Main_ exposing (..)\n\nimport Frontend\nimport Backend\nimport Types\nimport Html\n\nmain = Html.text \"\""

  r <- async $
    Dir.withCurrentDirectory root $
      Make.run [scaffold] $
        Make.Flags
          { _debug = True
          , _optimize = False
          , _output = Just (Make.JS tmp)
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  remove tmp
  remove scaffold

  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10
