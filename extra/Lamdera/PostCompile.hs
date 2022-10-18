{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PostCompile where

import qualified Build

import System.FilePath ((</>))

import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Elm.Details as Details

import qualified Ext.ElmPages
import qualified Ext.Query.Interfaces
import Lamdera


check :: Details.Details -> Build.Artifacts -> (Exit.BuildProblem -> b) -> Task.Task b ()
check details artifacts errorWrapper =
  Task.eio errorWrapper $ do
    root <- getProjectRoot "Lamdera.PostCompile.check"
    hasElmPagesPageData <- fileContains (root </> ".elm-pages/Main.elm") "type PageData"
    if hasElmPagesPageData
      then checkElmPagesTypes details artifacts
      else pure $ Right ()



checkElmPagesTypes :: Details.Details -> Build.Artifacts -> IO (Either Exit.BuildProblem ())
checkElmPagesTypes details artifacts = do
  -- Unfortunately Build.Artifacts only contains project deps, we need the full tree...
  interfaces <- Ext.Query.Interfaces.artifactsToFullInterfaces details artifacts

  case Ext.ElmPages.checkPageDataType interfaces of
    Right _  -> pure $ Right ()
    Left err -> pure $ Left err