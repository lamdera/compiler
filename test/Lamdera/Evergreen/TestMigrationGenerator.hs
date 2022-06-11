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
  [ scope "primitive migration" $ do

      let filenames = ["src/Evergreen/V1/Types.elm", "src/Evergreen/V2/Types.elm"]
      io $ Lamdera.Compile.makeDev "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate" filenames

      result <- io $ betweenVersions 1 2 [("BackendModel", "oldhash", "newhash")]

    --     nextVersion = (WithMigrations 2)
    --     migrationsFilenames = ["V2.elm"]
    --   migrations <- io $ getMigrationsSequence migrationsFilenames nextVersion 2
    --   result <- io $ withProdMode $ lamderaGenerated nextVersion migrationsFilenames



      expectEqualTextTrimmed result
        [text|
          module Evergreen.Migrate.V2 exposing (..)

          import Browser
          import Http
          import Lamdera
          import Url


          frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
          frontendModel old = ModelUnchanged


          backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
          backendModel old =
              ...

        |]
  ]
