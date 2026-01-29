{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Deploy where

import System.Process
import Data.List
import Data.Text (pack, strip, unpack)
import Control.Monad

import qualified Reporting
import qualified Reporting.Doc as D

import Lamdera
import qualified Lamdera.Project
import qualified Lamdera.CLI.Check

run :: () -> () -> IO ()
run () () = do
  branch <- Lamdera.getGitBranch
  case branch of
    b | b == "main" || b == "master" -> do
      debug_ "Starting check..."
      Lamdera.CLI.Check.run_
      Lamdera.CLI.Check.progressPointer "Pushing to lamdera..."
      _ <- readProcess "git" ["push", "lamdera", unpack b] ""
      pure ()

    _ -> do
      let branchName = Lamdera.Project.makeNameClean branch
      appName <- Lamdera.Project.appNameOrThrow
      approveReset <- Reporting.ask $
        D.fillSep
          [ D.dullyellow $ "Non-master deploys will create/update a preview app.\n\n"
          , "See the docs for more info: https://dashboard.lamdera.app/docs/previews\n"
          , D.dullyellow $ "Do you want to create/replace:"
          , D.blue $ "https://" <> D.fromChars (unpack appName) <> "-" <> D.fromChars (unpack branchName) <> ".lamdera.app"
          , "?"
          , "[Y/n]: "
          ]

      when approveReset $ do
        if Lamdera.isDebug_
          then System.Process.readProcess "git" ["push", "preview", "-f"] ""
          else System.Process.readProcess "git" ["push", "lamdera", "-f"] ""
        pure ()
