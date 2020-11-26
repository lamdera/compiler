{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Deploy where

import Lamdera
import System.Process

import qualified Lamdera.CLI.Check

run :: () -> () -> IO ()
run () () = do
  debug_ "Starting check..."

  Lamdera.CLI.Check.run () ()

  _ <- readProcess "git" ["push", "lamdera", "master"] ""

  pure ()
