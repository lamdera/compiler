{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Deploy where

import Lamdera
import System.Process

import qualified Lamdera.Check

run :: () -> () -> IO ()
run () () = do
  debug_ "Starting check..."

  Lamdera.Check.run () ()

  _ <- readProcess "git" ["push", "lamdera", "master"] ""

  pure ()
