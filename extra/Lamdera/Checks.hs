{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Checks where

import qualified Data.Map as Map
import Data.Text as T
import Data.Text.IO as TIO
import qualified System.Directory as Dir
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT, liftIO)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import qualified Debug.Trace as DT
import qualified Data.List as List

import qualified Reporting.Doc as D
import qualified Elm.Package as Pkg
import Elm.ModuleName (Canonical(..))
import qualified Type.Error as T
import qualified Reporting
import qualified Reporting.Task as Task
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Elm.Version as V

import NeatInterpolation

import Lamdera
import Lamdera.Progress
import qualified Lamdera.Init
import qualified Ext.Common


runChecks :: FilePath -> Bool -> Map.Map Pkg.Name V.Version -> IO (Either Exit.Outline outline) -> IO (Either Exit.Outline outline)
runChecks root shouldCheckLamdera direct default_ = do
  -- atomicPutStrLn $ "runchecks but with " <> show shouldCheckLamdera
  if Map.member Pkg.lamderaCore direct
    then do
      onlyWhen shouldCheckLamdera (Lamdera.Checks.runChecks_ root)
      default_
    else
      if shouldCheckLamdera
        then return $ Left Exit.OutlineLamderaMissingDeps
        else default_


runChecks_ :: FilePath -> IO ()
runChecks_ root = do

  missingFiles <- liftIO $ checkMissingFiles root ["src/Frontend.elm", "src/Backend.elm", "src/Types.elm"]
  unless (missingFiles == []) $ do

    let
      formattedErrors = missingFiles & fmap (\file -> D.reflow $ "- " <> file )

    if List.length missingFiles == 3
      then do

      initialiseLamderaFiles <- Reporting.ask $
        Help.reportToDoc $
          Help.report "MISSING FILES" (Nothing)
            ("The following files required by Lamdera are missing:")
            (formattedErrors ++
            [ D.reflow $ "It looks like you're starting from scratch!"
            , D.reflow $ "Would you like me to create a starter implementation? [Y/n]: "
            ])

      if initialiseLamderaFiles
        then do
          liftIO $ Lamdera.Init.writeDefaultImplementations
          -- @TODO future
          -- It would be nice if when coming from an existing elm project, we installed the missing
          -- deps as well, but the UI impact is a little bit weird as it's not transparent to the user
          -- whats happening, and at the same time the runInstall headless helper we have causes cyclic
          -- deps so can't really use that until we unravel things...
          -- liftIO $ callCommand "elm install elm/url"
          -- This would be much better but we have cyclic dep issues
          -- Install.run (Install.Install (Package.Name.toName Package.Name.elm "url")) ()
          --
          -- Not a huge deal though, the user error explains that elm/url is missing so overall
          -- users should be able to unblock albeit it being a bit ugly a process.

          report $ D.dullgreen "Okay, I've generated them for you!\n"
        else
          throw
            $ Help.report "SKIPPING AUTO-GENERATION" (Nothing)
              ("Okay, I'll let you implement them!")
              [ D.reflow "See <https://dashboard.lamdera.app/docs/building> for more."]

      else

        throw
          $ Help.report "MISSING FILES" (Nothing)
              ("The following files required by Lamdera are missing:")
              (formattedErrors ++
              [ D.reflow "It looks like you've already started implementing things!"
              , D.reflow "See <https://dashboard.lamdera.app/docs/building> for more."
              ])


  -- Ensure Env.elm is in place before we get going, otherwise
  -- our `mode` value injection will fail spectacularly
  envExists <- liftIO $ doesFileExist "src/Env.elm"

  onlyWhen (not envExists) $ do
    liftIO $ writeUtf8 "src/Env.elm" Lamdera.Init.emptyEnv
    progressPointer "Created empty Env.elm"


progressPointer t =
    report $ D.fillSep [ D.fromChars "───>", D.dullgreen $ t <> "\n" ]



checkMissingFiles :: FilePath -> [FilePath] -> IO [FilePath]
checkMissingFiles root paths = do
  exists <- mapM (\path -> do
      exist <- Dir.doesFileExist (root </> path)
      if exist then pure (True, path) else pure (False, path)
    ) paths

  let
    missingFilePairs =
      Prelude.filter (\(exists, path) -> exists == False) exists

    missingFilePaths =
      Prelude.map (\(exists, path) -> path) missingFilePairs

  pure missingFilePaths


checkHasAppDefinition :: FilePath -> IO Bool
checkHasAppDefinition path = do
  source <- TIO.readFile path
  pure $ T.isInfixOf "app =" source


checkMsgHasTypes :: [Text] -> IO Bool
checkMsgHasTypes typeNames = do
  source <- TIO.readFile "src/Types.elm"

  let
    results = fmap (\search -> T.isInfixOf ("type " <> search) source) typeNames

  pure $ Prelude.all ((==) True) results
