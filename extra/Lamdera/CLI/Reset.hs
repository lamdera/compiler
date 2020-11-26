{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Reset where

import qualified Data.Text as T
import qualified Data.List as List
import System.FilePath ((</>))

import qualified Stuff as PerUserCache
import qualified Reporting
import qualified Reporting.Doc as D

import Lamdera
import Lamdera.Progress


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting reset..."

  elmHome <- PerUserCache.getElmHome
  root <- getProjectRoot
  let elmStuff = root </> "elm-stuff"

  progress "Here is the plan:\n"

  report $ D.fillSep ["-", D.red "Remove", D.fromChars elmStuff]
  report $ D.fillSep ["-", D.red "Remove", D.fromChars elmHome]

  progress ""

  approveReset <- Reporting.ask $
    D.fillSep [ "Shall I proceed?", D.red "(this cannot be undone)", "[Y/n]: " ]

  if approveReset
    then do
      progress $ "\nRemoving " <> elmStuff
      rmdir $ elmStuff
      progress $ "Removing " <> elmHome
      rmdir $ elmHome

    else
      progress "\nOkay, I did not reset."

  pure ()
