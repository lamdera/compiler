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

  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "git" ["branch","--show-current"] ""

  let branch = stdout & pack & strip & unpack

  case branch of
    "main" -> do
      _ <- readProcess "git" ["push", "lamdera", "main"] ""
      pure ()

    "master" -> do
      _ <- readProcess "git" ["push", "lamdera", "master"] ""
      pure ()

    _ ->
      atomicPutStrLn $ "Error: deploys must be from a main or master branch. You are on branch " <> branch <> "."

  pure ()
