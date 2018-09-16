{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  , generateDocs
  )
  where


import qualified Data.ByteString as BS
import Data.Map ((!))
import System.FilePath ((</>), makeRelative)

import qualified Elm.Compiler as Compiler
import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Output as Output
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path
import Control.Monad.Trans (liftIO)
import Text.Show.Prettyprint

import qualified Elm.PerUserCache as PerUserCache
import Data.Text
import Data.Monoid ((<>))
import Data.Aeson.Types as Aeson
import qualified Elm.Package as Pkg
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Yaml
import qualified System.Directory as Dir
import qualified Deps.Verify as Verify

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import qualified Elm.Project.Constraint as Con
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import Elm.Project.Licenses (License(License))
import qualified Elm.Project.Json as ElmJson

import qualified Elm.Package as Pkg
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import Elm.Project.Licenses (License(License))
import qualified Elm.Project.Constraint as Con
import qualified Elm.Name as N
import qualified Elm.Project.Json as ElmJson
import Data.Monoid ((<>))
import qualified Elm.Package as EP
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Elm.Compiler.Module
import qualified Haskelm.Output

import qualified Debug.Trace as DT

type List a = [a]

-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile
  :: Output.Mode
  -> Output.Target
  -> Maybe Output.Output
  -> Maybe FilePath
  -> Summary
  -> [FilePath]
  -> Task.Task ()
compile mode target maybeOutput docs summary@(Summary.Summary root project _ _ _) paths =
  do  Project.check project
      args <- Args.fromPaths summary paths
      -- debugTo "args.txt" args
      graph <- Crawl.crawl summary args
      -- debugTo "graph.txt" graph
      (dirty, ifaces) <- Plan.plan docs summary graph
      -- debugTo "dirty.txt" dirty
      -- debugTo "ifaces.txt" ifaces

      answers <- Compile.compile project docs ifaces dirty
      -- debugTo "answers.txt" answers
      results <- Artifacts.write root answers -- results : Map ModuleName Artifacts, where Artifacts = {elmInterface, elmOutput (graph), docs}
      -- debugTo "results.txt" results

      _ <- Haskelm.Output.generateHaskellCodeAndYamlFiles root project graph results

      _ <- traverse (Artifacts.writeDocs results) docs
      Output.generate mode target maybeOutput summary graph results


debugTo fname a = do
  liftIO $ print $ "-------------------------------------------------------------------" ++ fname
  liftIO $ writeFile fname $ prettyShow a


-- COMPILE FOR REPL


compileForRepl :: Bool -> L.Localizer -> BS.ByteString -> Maybe N.Name -> Task.Task (Maybe FilePath)
compileForRepl noColors localizer source maybeName =
  do  summary@(Summary.Summary root project _ _ _) <- getRoot
      Project.check project
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan Nothing summary graph
      answers <- Compile.compile project Nothing ifaces dirty
      results <- Artifacts.write root answers
      let (Compiler.Artifacts elmi _ _ _) = results ! N.replModule
      traverse (Output.generateReplFile noColors localizer summary graph elmi) maybeName



-- GENERATE DOCS


generateDocs :: Summary.Summary -> Task.Task Docs.Documentation
generateDocs summary@(Summary.Summary root project _ _ _) =
  do  let docsPath = root </> Path.docs
      args <- Args.fromSummary summary
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan (Just docsPath) summary graph
      answers <- Compile.compile project (Just docsPath) ifaces dirty
      results <- Artifacts.write root answers
      Output.noDebugUsesInPackage summary graph
      Artifacts.writeDocs results docsPath
