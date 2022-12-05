{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Evergreen.TestMigrationGenerator where

import EasyTest
import Control.Applicative
import Control.Monad
import NeatInterpolation
import qualified Data.Text as T

import Test.Helpers
import qualified Ext.Common

import Lamdera
import qualified Lamdera.Compile
import qualified Lamdera.Types
import qualified Lamdera.Evergreen.MigrationGenerator


all = do
  run suite


suite :: Test ()
suite = tests
  [ scope "primitive migration: 1 -> 2" $ testMigrationGeneration "scenario-migration-generate" 1 2
  ]


testMigrationGeneration scenario oldVersion newVersion = do

  io $ atomicPutStrLn <$> Ext.Common.requireBinary "elm-format"

  let root = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"
      typeCompares = zipWith3
        (\label local prod -> (label, T.unpack local, T.unpack prod))
        (Lamdera.Types.core)
        ["o","o","o","o","o","o"]
        ["x","x","x","o","o","o"]


  mock <- io $ readUtf8Text $ "test/scenario-migration-generate/src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
  result <- io $ Lamdera.Evergreen.MigrationGenerator.betweenVersions typeCompares oldVersion newVersion root

  expectEqualTextTrimmed (mock & withDefault "failed to load file") result

  let filenames =
        [ "src/Evergreen/V" <> show oldVersion <> "/Types.elm"
        , "src/Evergreen/V" <> show newVersion <> "/Types.elm"
        , "src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
        ]

  actual <- catchOutput $
    Lamdera.Compile.makeDev "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate" filenames

  expectTextContains actual
    "This `Unimplemented` value is a:\n\n    UnimplementedMigration"
