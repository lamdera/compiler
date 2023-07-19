{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Deploy where

import System.Process
import Data.List
import Data.Text (pack, strip, unpack)

import qualified Reporting
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Project
import qualified Lamdera.CLI.Check

run :: () -> () -> IO ()
run () () = do
  branch <- Lamdera.getGitBranch
  case branch of
    "main" -> do
      debug_ "Starting check..."
      Lamdera.CLI.Check.run_
      _ <- readProcess "git" ["push", "lamdera", "main"] ""
      pure ()

    "master" -> do
      debug_ "Starting check..."
      Lamdera.CLI.Check.run_
      _ <- readProcess "git" ["push", "lamdera", "master"] ""
      pure ()

    branchName -> do
      appName <- Lamdera.Project.appNameOrThrow
      approveReset <- Reporting.ask $
        D.fillSep
          [ "Non-master deploys will create/update a preview app.\n\nSee the docs for more info: https://dashboard.lamdera.app/docs/previews\n"
          , D.dullyellow $ "Do you want to create/replace"
          , D.blue "https://" <> D.fromChars (unpack appName) <> "-" <> D.fromChars (unpack branchName) <> ".lamdera.app"
          , "?"
          , "[Y/n]: "
          ]

      if approveReset
        then do
          if Lamdera.isDebug_
            then System.Process.readProcess "git" ["push", "preview", "-f"] ""
            else System.Process.readProcess "git" ["push", "lamdera", "-f"] ""
          pure ()
        else
          pure ()

  pure ()
