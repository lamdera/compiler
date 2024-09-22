{-# LANGUAGE OverloadedStrings #-}

-- lamdera update – to update the lamdera binary
-- lamdera ugprade namespace left for potential future command to mirror elm-json upgrade

module Lamdera.CLI.Update where

import Control.Exception (bracket_)
import Data.List (isPrefixOf)
import Network.HTTP.Client
import qualified Data.Text as T
import qualified System.Info
import qualified Text.Read
import System.Directory (renameFile, getPermissions, setPermissions, executable)
import System.Environment (getExecutablePath, getProgName)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

import qualified Http
import qualified Reporting
import qualified Reporting.Doc as D

import Lamdera
import Lamdera.Version (Version)
import qualified Lamdera.Http
import qualified Lamdera.Update
import qualified Lamdera.Version


newtype Flags =
  Flags
    { _force :: Bool
    }


run :: () -> Flags -> IO ()
run _ flags@(Flags force) = do
  binaryPath <- getExecutablePath

  onlyWhen (isInNixStore binaryPath) $ error $
    "Oops! Looks like lamdera is installed via Nix (" <> binaryPath <> ").\n" <>
    "Skipping self-upgrade, please bump your Nix derivation manually."

  onlyWhen (isRunningInGHCi binaryPath) $ error $
    "Oops! Can't self-update in GHCi (detected by current path " <> binaryPath <> ")"

  latest <- Lamdera.Update.getLatestVersion

  case latest of
    Left err -> do
      atomicPutStrLn $ "Error fetching latest version: " <> show err

    Right latestVersion -> do
      if not (Lamdera.Update.isLatest latestVersion) || force
        then do
          updateApproved <- do
            Reporting.ask $ D.stack [ D.reflow $
              "Replace " <> binaryPath <> " with version " <> Lamdera.Version.rawToString latestVersion <> "? [Y/n]: "
              ]

          onlyWhen updateApproved $ do
            atomicPutStrLn $ "Downloading Lamdera from " <> downloadUrl latestVersion
            withSystemTempDirectory "lamdera-upgrade" $ \tempDir -> do
              let tempFilePath = tempDir </> "lamdera-new"
              download <- Lamdera.Http.downloadToFile (downloadUrl latestVersion) tempFilePath
              case download of
                Left err -> do
                  atomicPutStrLn $ "Error downloading latest version: " <> show err
                Right () -> do
                  overwriteBinary tempFilePath binaryPath
                  execNewBinary binaryPath

        else
          atomicPutStrLn $ "No updates available, version " <> Lamdera.Version.rawToString latestVersion <> " is the latest."


downloadUrl :: Version -> String
downloadUrl version =
  let
    base = "https://static.lamdera.com/bin/lamdera"

    architecture =
      case System.Info.arch of
        "aarch64" -> "arm64"
        "x86_64" -> "x86_64"
        _ -> error "Oops! Unsupported architecture: " <> System.Info.arch <> ", please report this."
  in
  case ostype of
      MacOS -> do
        base <> "-" <> Lamdera.Version.rawToString version <> "-" <> "macos" <> "-" <> architecture

      Linux -> do
        base <> "-" <> Lamdera.Version.rawToString version <> "-" <> "linux" <> "-" <> architecture

      Windows -> do
        error "Oops, I can't auto-update on Windows, please download manually from https://dashboard.lamdera.app/docs/download"

      UnknownOS name -> do
        error $ "Oops, I couldn't figure out the OS to upgrade for. Please report unknown OSTYPE: " <> show name


overwriteBinary :: FilePath -> FilePath -> IO ()
overwriteBinary tempFilePath destFilePath = do
  renameFile tempFilePath destFilePath
  permissions <- getPermissions destFilePath
  setPermissions destFilePath (permissions { executable = True })


execNewBinary :: FilePath -> IO ()
execNewBinary binaryPath = do
  callProcess binaryPath []


isInNixStore :: FilePath -> Bool
isInNixStore path = path & stringHasPrefix "/nix/store"


isRunningInGHCi :: String -> Bool
isRunningInGHCi binaryPath =
  binaryPath & stringContains "/ghc/"
