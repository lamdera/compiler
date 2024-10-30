{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Legacy where

import System.FilePath ((</>))
import qualified System.Directory as Dir

import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Stuff

import Lamdera
import qualified Lamdera.Progress as Progress


-- Applies to < v1.3.0
temporaryCheckCodecsNeedsUpgrading :: Bool -> FilePath -> IO ()
temporaryCheckCodecsNeedsUpgrading inProduction root = do
  elmHome <- Stuff.getElmHome
  let
    elmStuff = root </> "elm-stuff"
    lamderaCodecs = elmHome </> "0.19.1/packages/lamdera/codecs/1.0.0"
    lamderaMigrations = lamderaCodecs </> "/src/Lamdera/Migrations.elm"
  exists_ <- Dir.doesFileExist lamderaMigrations
  latest <- fileContains lamderaMigrations "ModelReset"
  onlyWhen (exists_ && not latest) $ do
    rmdir lamderaCodecs
    rmdir elmStuff


-- Applies to < alpha5
temporaryCheckOldTypesNeedingMigration :: Bool -> FilePath -> IO ()
temporaryCheckOldTypesNeedingMigration inProduction root = do

  exists_ <- Dir.doesDirectoryExist $ root </> "src" </> "Evergreen" </> "Type"

  onlyWhen exists_ $ do

    if inProduction
      then
        Progress.throw
          $ Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.dullyellow $ D.reflow "Please download the latest binary and run `lamdera check` again."
               , D.reflow $ "https://dashboard.lamdera.app/docs/download"
               , D.reflow $ "See the full release here: https://dashboard.lamdera.app/releases/alpha5"
               ]
              )
      else
        Progress.report $
            Help.reportToDoc $
            Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.reflow "The following changes were introduced:"
               , D.vcat
                   [ D.reflow $ "- Type snapshots now extract from your entire project, not just Types.elm"
                   , D.reflow $ "- Type snapshots now live in src/Evergreen/V*/ folders for each version"
                   , D.reflow $ "- Only the types referenced by the 6 core types will get extracted (i.e. functions/other types no longer get copied)"
                   ]
               , D.reflow $ "See the full release here: https://dashboard.lamdera.app/releases/alpha5"
               , D.dullyellow $ D.reflow $ "I can help you migrate by doing the following:"
               , D.vcat
                   [ D.reflow $ "- Moving src/Evergreen/Type/V*.elm to src/Evergreen/V*/Types.elm"
                   , D.reflow $ "- Renaming all the moved module names"
                   , D.reflow $ "- Renaming all Type imports in migrations"
                   , D.reflow $ "- Removing the src/Evergreen/Type/ folder"
                   , D.reflow $ "- Staging the changes for git commit"
                   ]
               ]
              )

    migrationApproved <- Reporting.ask $
      D.stack
        [ D.reflow $ "Shall I attempt to migrate for you? [Y/n]: "
        ]

    if migrationApproved
      then do

        let oldTypeSnapshotFolder = root </> "src/Evergreen/Type"

        typeFilepaths <- safeListDirectory $ oldTypeSnapshotFolder

        typeFilepaths
          & mapM (\filepath -> do

            case getVersion filepath of
              Just version -> do
                let dest = (root </> "src" </> "Evergreen" </> ("V" <> show version) </> "Types.elm")

                putStrLn $ "Moving " <> filepath <> " -> " <> "src/Evergreen/V" <> show version <> "/Types.elm"
                copyFile (oldTypeSnapshotFolder </> filepath) dest

                putStrLn $ "Renaming '" <> ("module Evergreen.Type.V" <> show version) <> "' to '" <> ("module Evergreen.V" <> show version <> ".Types") <> "'"
                replaceInFile ("module Evergreen.Type.V" <> (show_ version)) ("module Evergreen.V" <> (show_ version) <> ".Types") dest

                callCommand $ "git add " <> dest

              Nothing ->
                -- Skip any incorrectly named files...
                pure ()
          )

        putStrLn $ "Removing " <> (oldTypeSnapshotFolder) <> "..."
        rmdir $ root </> "src/Evergreen/Type"


        putStrLn $ "Renaming migration imports..."

        migrationFilepaths <- safeListDirectory $ root </> "src/Evergreen/Migrate"

        migrationFilepaths
          & mapM (\filepath -> do

            case getVersion filepath of
              Just version -> do

                -- import Evergreen.Type.V18 as Old
                -- import Lamdera.Migrations exposing (..)
                -- import Evergreen.Type.V20 as New
                let migrationPath = (root </> "src/Evergreen/Migrate" </> filepath)

                putStrLn $ "Renaming imports in " <> migrationPath

                replaceInFile ("import Evergreen.Type.") ("import Evergreen.") migrationPath
                replaceInFile (" as Old") (".Types as Old") migrationPath
                replaceInFile (" as New") (".Types as New") migrationPath

              Nothing ->
                -- Skip any incorrectly named files...
                pure ()
          )

        putStrLn $ "Staging the changes for git commit..."
        callCommand $ "git add -u " <> oldTypeSnapshotFolder <> " || true"

        putStrLn $ "\n\nDone! If you encounter issues with this helper, please drop a note in Discord.\n\n"

        pure ()

      else
        genericExit "Okay, I did not migrate."


genericExit :: String -> IO a
genericExit str =
  Progress.throw
    $ Help.report "ERROR" Nothing
      (str)
      []
