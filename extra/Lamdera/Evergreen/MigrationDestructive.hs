{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Evergreen.MigrationDestructive where

import NeatInterpolation
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Name as N

import qualified Ext.ElmFormat
import Lamdera
import Lamdera.Evergreen.MigrationGeneratorHelpers


generate :: Int -> Int -> [(N.Name, String, String)] -> IO Text
generate oldVersion newVersion typeCompares = do
  let old = show_ oldVersion
      new = show_ newVersion

      typeCompareMigration :: (N.Name, String, String) -> Text
      typeCompareMigration (typename, oldhash, newhash) = do
        let implementation = destructiveForType typename
            msgType = msgForType typename
            typenameCamel = lowerFirstLetter $ N.toChars typename
            typenameT = N.toText typename
            migrationType = migrationTypeForType typename

        [text|
          $typenameCamel : Old.$typenameT -> $migrationType New.$typenameT New.$msgType
          $typenameCamel old =
              $implementation
        |]

  let header = [text|

    module Evergreen.Migrate.V$new exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)

    import Evergreen.V$old.Types as Old
    import Evergreen.V$new.Types as New
    import Lamdera.Migrations exposing (..)


  |]

  typeCompares
    & fmap typeCompareMigration
    & (<>) [header]
    & T.intercalate "\n\n\n"
    & Ext.ElmFormat.formatOrPassthrough
    & pure
