{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Preview where

import Lamdera
import qualified System.Process
import Data.Text (unpack)

import qualified Lamdera.CLI.Check

run :: () -> () -> IO ()
run () () = do
  branch <- Lamdera.getGitBranch
  let needsBranch = atomicPutStrLn $
        "⚠️  Error: previews must be from a non-main/master branch. You are on branch `" <> unpack branch <> "`. Use `git checkout -b <branchname>` to create a new branch."

  case branch of
    "main" ->   needsBranch
    "master" -> needsBranch
    _ -> do
      _ <-
        if Lamdera.isDebug_
          then System.Process.readProcess "git" ["push", "preview", "-f"] ""
          else System.Process.readProcess "git" ["push", "lamdera"] ""
      pure ()

  pure ()
