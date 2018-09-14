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
import System.FilePath ((</>), makeRelative, splitPath, joinPath)

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
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.Aeson.Types as Aeson
import qualified Elm.Package as Pkg
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Yaml

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

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
      debugTo "graph.txt" graph
      (dirty, ifaces) <- Plan.plan docs summary graph
      -- debugTo "dirty.txt" dirty
      -- debugTo "ifaces.txt" ifaces

      -- write stack.yaml
      stackFileContents <- stackYaml root graph
      liftIO $ encodeFile (root </> "stack.yaml") stackFileContents

      answers <- Compile.compile project docs ifaces dirty
      -- debugTo "answers.txt" answers
      results <- Artifacts.write root answers -- results : Map ModuleName Artifacts, where Artifacts = {elmInterface, elmOutput (graph), docs}
      -- debugTo "results.txt" results

      _ <- traverse (Artifacts.writeDocs results) docs
      Output.generate mode target maybeOutput summary graph results


debugTo fname a = do
  liftIO $ print $ "-------------------------------------------------------------------" ++ fname
  liftIO $ writeFile fname $ prettyShow a


-- GENERATE STACK.YAML

stackYaml :: FilePath -> Crawl.Graph a b -> Task.Task StackYaml
stackYaml
  root
  (Crawl.Graph
  _args
  _locals
  _kernels
  _foreigns
  _problems) =
    let
      pkgs =
        _foreigns
        & Map.elems
        & removeDuplicates
        & mapM (\(Pkg.Package name version) ->
          do
            cacheDir <- Task.getPackageCacheDirFor name version
            let absPath = (cacheDir </> "haskelm")
            let relPath = makeRelative root absPath
            --DT.trace (show ("abs", absPath, "rel", relPath, "root", root)) $
            pure $ pack relPath
          )
      extDeps =
        let
          getSubdirs :: FilePath -> IO [FilePath]
          getSubdirs d =
            do
              contents <- listDirectory d
              let contentPaths = (d </>) <$> contents
              subDirs <- filterM (doesDirectoryExist) contentPaths
              pure subDirs
        in
        do
          cacheDir <- Task.getPackageCacheDir
          sub1 <- liftIO $ getSubdirs cacheDir
          sub2 <- liftIO $ getSubdirs `mapM` sub1
          sub3 <- liftIO $ getSubdirs `mapM` (Prelude.concat sub2)

          let parentOfCacheDir = cacheDir & splitPath & Prelude.reverse & Prelude.drop 1 & Prelude.reverse & joinPath
          let haskelm = parentOfCacheDir </> "lamdera" </> "haskelm"
          let dirs = (</> "haskelm") <$> Prelude.concat sub3
          pure $ pack <$> (haskelm : dirs)
    in do
      p <- pkgs
      e <- extDeps
      pure $ StackYaml p e

removeDuplicates = Prelude.foldr (\x seen -> if x `elem` seen then seen else x : seen) []

-- data Graph kernel problems =
--   Graph
--     { _args :: Args.Args Module.Raw
--     , _locals :: Map.Map Module.Raw Header.Info
--     , _kernels :: Map.Map Module.Raw kernel
--     , _foreigns :: Map.Map Module.Raw Pkg.Package
--     , _problems :: problems
--     }
--     deriving (Show)

-- ( Bitwise
-- , Package
--     { name =
--         Name
--           { author = "elm", project = "core" }
--     , version =
--         Version
--           { major = 1, minor = 0, patch = 0 }
--     }
-- )


data StackYaml =
  StackYaml
  { packages :: List Text
  , extDeps :: List Text
  }


instance ToJSON StackYaml where
  toJSON (StackYaml pkgs extDeps) =
    object
    [ "packages" .= array (Aeson.String <$> ["."])
    , "extra-deps" .= array (Aeson.String <$> (extDeps ++ lamderaDeps))
    -- hard-coded values
    , "resolver" .= Aeson.String "lts-11.9"
    , "require-stack-version" .= Aeson.String ">= 1.4.0"
    --, "flags" .= array ()
    --, "extra-package-dbs" .= array ()
    ]

lamderaDeps =
  [ "acid-state-0.14.3"
  , "regexpr-0.5.4"
  , "superrecord-0.5.0.1"
  , "unagi-chan-0.4.1.0"
  , "mtlparse-0.1.4.0"
  ]


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
