{-# LANGUAGE OverloadedStrings #-}

module Lamdera.PostCompile where

import qualified Build

import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task

import qualified Ext.ElmPages
import qualified Ext.Query.Interfaces
import Lamdera


check :: Build.Artifacts -> (Exit.BuildProblem -> b) -> Task.Task b ()
check artifacts errorWrapper =
  Task.eio errorWrapper $ do
    hasElmPagesPageData <- fileContains ".elm-pages/Main.elm" "type PageData"
    if hasElmPagesPageData
      then checkElmPagesTypes artifacts
      else pure $ Right ()



checkElmPagesTypes :: Build.Artifacts -> IO (Either Exit.BuildProblem ())
checkElmPagesTypes artifacts = do
  inDebug <- isDebug
  -- Unfortunately Build.Artifacts only contains project deps, we need the full tree...
  interfaces <- Ext.Query.Interfaces.artifactsToFullInterfaces artifacts

  case Ext.ElmPages.checkPageDataType interfaces inDebug of
    Right _  -> pure $ Right ()
    Left err -> pure $ Left err