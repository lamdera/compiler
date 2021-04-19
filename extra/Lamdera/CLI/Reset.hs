{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Reset where

import qualified Data.Text as T
import qualified Data.List as List
import System.FilePath ((</>))
import qualified System.Directory as Dir

import qualified Stuff as PerUserCache
import qualified Reporting
import qualified Reporting.Doc as D

import Lamdera
import Lamdera.Progress


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting reset..."

  elmHome <- PerUserCache.getElmHome
  root <- getProjectRootMaybe

  let
    elmStuff = (root & withDefault "./") </> "elm-stuff"
    lamderaLegacy = (root & withDefault "./") </> "lamdera-stuff"

  progress "Here is the plan:\n"

  report $ D.fillSep ["-", D.red "Remove", D.fromChars elmHome]

  onlyWhen_ (doesDirectoryExist elmStuff) $ do
    report $ D.fillSep ["-", D.red "Remove", D.fromChars elmStuff]

  onlyWhen_ (doesDirectoryExist lamderaLegacy) $
    report $ D.fillSep ["-", D.red "Remove", D.fromChars lamderaLegacy, "(legacy)"]

  legacyLamderaHome <- Dir.getAppUserDataDirectory "lamdera"
  onlyWhen_ (doesDirectoryExist legacyLamderaHome) $
    report $ D.fillSep ["-", D.red "Remove", D.fromChars legacyLamderaHome, "(legacy)"]

  progress ""

  onlyWhen_ (not <$> doesDirectoryExist elmStuff) $ do
    report $ D.fillSep [D.red "Warning:", "you're","not","in","an","Elm","project","folder,", "so","I","can","only","reset","the","global","Elm","cache."]
    progress ""

  approveReset <- Reporting.ask $
    D.fillSep [ "Shall I proceed?", D.red "(this cannot be undone)", "[Y/n]: " ]

  if approveReset
    then do
      progress $ "Removing " <> elmHome
      rmdir elmHome

      onlyWhen_ (doesDirectoryExist elmStuff) $ do
        progress $ "Removing " <> elmStuff
        rmdir elmStuff

      onlyWhen_ (doesDirectoryExist lamderaLegacy) $ do
        progress $ "Removing " <> lamderaLegacy
        rmdir lamderaLegacy

      onlyWhen_ (doesDirectoryExist legacyLamderaHome) $ do
        progress $ "Removing " <> legacyLamderaHome
        rmdir legacyLamderaHome


    else
      progress "\nOkay, I did not reset."

  pure ()
