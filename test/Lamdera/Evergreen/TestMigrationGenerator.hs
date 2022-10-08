{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Evergreen.TestMigrationGenerator where

import EasyTest
import Control.Applicative
import Control.Monad
import NeatInterpolation
import qualified Data.Text as T
import System.Environment (setEnv, unsetEnv)

import Test.Helpers

import Lamdera
import qualified Lamdera.Compile
import Lamdera.Evergreen.MigrationGenerator


all = do
  run suite


suite :: Test ()
suite = tests
  [ scope "primitive migration: 1 -> 2" $ testMigrationGeneration "scenario-migration-generate" 1 2
  ]


testMigrationGeneration scenario oldVersion newVersion = do

  mock <- io $ readUtf8Text $ "test/scenario-migration-generate/src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
  result <- io $ betweenVersions oldVersion newVersion

  expectEqualTextTrimmed (mock & withDefault "failed to load file") result

  let filenames =
        [ "src/Evergreen/V" <> show oldVersion <> "/Types.elm"
        , "src/Evergreen/V" <> show newVersion <> "/Types.elm"
        , "src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
        ]
  io $ Lamdera.Compile.makeDev "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate" filenames