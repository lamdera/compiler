{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Deploy where

import Lamdera
import System.Process
import Data.List
import Data.Text (pack, strip, unpack)


import qualified Lamdera.CLI.Check

run :: () -> () -> IO ()
run () () = do
  debug_ "Starting check..."

  Lamdera.CLI.Check.run () ()

  -- This invocation doesn't appear to work on older git versions, left for posterity
  -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "git" ["branch","--show-current"] ""
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "git" ["symbolic-ref", "--short", "-q", "HEAD"] ""
  let branch = stdout & pack & strip

  case branch of
    "main" -> do
      _ <- readProcess "git" ["push", "lamdera", "main"] ""
      pure ()

    "master" -> do
      _ <- readProcess "git" ["push", "lamdera", "master"] ""
      pure ()

    _ ->
      atomicPutStrLn $ "⚠️  Error: deploys must be from a main or master branch. You are on branch " <> unpack branch <> "."

  pure ()
