{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Reset where

import qualified Data.Text as T
import qualified Data.List as List
import System.FilePath ((</>))
import qualified System.Directory as Dir

import qualified Stuff as PerUserCache
import qualified Reporting
import qualified Reporting.Doc as D
import LamderaSharedBuildHelpers

import Lamdera
import Lamdera.Progress
import qualified Lamdera.Version


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting reset..."

  elmHome <- PerUserCache.getElmHome
  legacyElmHome <- Dir.getAppUserDataDirectory "lamdera"
  root <- getProjectRootMaybe
  let
    elmStuff = (root & withDefault "./") </> "elm-stuff"
    lamderaLegacy = (root & withDefault "./") </> "lamdera-stuff"
    lamderaCliLogin = elmHome </> ".lamdera-cli"

  progress "Here is the plan:\n"

  if List.isInfixOf [ostype] [MacOS, Linux]
    then
      report $ D.fillSep ["-", D.yellow "Remove artifacts in", D.fromChars elmHome]
    else
      planNukeDir elmHome ""

  planNukeFile lamderaCliLogin ""
  planNukeDir elmStuff ""
  planNukeDir legacyElmHome "(Legacy)"
  planNukeDir lamderaLegacy "(Legacy)"

  progress ""

  onlyWhen_ (not <$> doesDirectoryExist elmStuff) $ do
    report $ D.fillSep [D.red "Warning:", "you're","not","in","an","Elm","project","folder,", "so","I","can","only","reset","the","global","Elm","cache."]
    progress ""

  approveReset <- Reporting.ask $
    D.fillSep [ "Shall I proceed?", D.red "(this cannot be undone)", "[Y/n]: " ]

  if approveReset
    then do
      if List.isInfixOf [ostype] [MacOS, Linux]
        then do
          progress $ "Removing artifacts in " <> elmHome
          let packageDir = elmHome </> Lamdera.Version.elm </> "packages"
          onlyWhen_ (doesDirectoryExist packageDir) $ do
            c $ "find " <> packageDir <> " | grep artifacts.dat | xargs rm -r"
            c $ "find " <> packageDir <> " | grep artifacts.x.dat | xargs rm -r"
        else do
          nukeDir elmHome

      nukeFile lamderaCliLogin
      nukeDir elmStuff
      nukeDir legacyElmHome
      nukeDir lamderaLegacy

    else
      progress "\nOkay, I did not reset anything."

  pure ()


planNukeDir dir suffix =
  onlyWhen_ (doesDirectoryExist dir) $ do
    report $ D.fillSep ["-", D.red "Remove", D.fromChars dir, suffix]

planNukeFile file suffix =
  onlyWhen_ (doesFileExist file) $ do
    report $ D.fillSep ["-", D.red "Remove", D.fromChars file, suffix]

nukeDir dir =
  onlyWhen_ (doesDirectoryExist dir) $ do
    progress $ "Removing " <> dir
    rmdir dir

nukeFile file =
  onlyWhen_ (doesFileExist file) $ do
    progress $ "Removing " <> file
    remove file
