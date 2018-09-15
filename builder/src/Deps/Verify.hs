{-# LANGUAGE OverloadedStrings #-}

module Deps.Verify
  ( verify
  , HPackYaml(..)
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (filterM, void)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Map.Merge.Strict as Map
import Data.Map (Map)
import Data.Set (Set)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Docs as Docs
import Elm.Package (Name, Version)
import qualified Elm.PerUserCache as PerUserCache
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Json.Encode as Encode

import qualified Deps.Cache as Cache
import qualified Deps.Explorer as Explorer
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Compiler as Compiler
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Summary as Summary
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified File.Plan as Plan
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Deps as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths

import qualified Elm.Package as Pkg
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty

import Elm.Project.Licenses (License(License))
import qualified Elm.Project.Constraint as Con
import qualified Elm.Name as N
import qualified Elm.Project.Json as ElmJson

import Data.Yaml
import Data.Text
import Data.Monoid ((<>))
import Data.Aeson.Types as Aeson
import qualified Elm.Package as EP

type List a = [a]

-- VERIFY


verify :: FilePath -> Project -> Task.Task Summary.Summary
verify root project =
  do  solution <-
        case project of
          Project.App info -> verifyApp info
          Project.Pkg info -> verifyPkg info

      (RawInfo infos ifaces) <- verifyArtifacts solution
      return (Summary.init root project infos ifaces)


throw :: E.Exit -> Task.Task a
throw exit =
  Task.throw (Exit.Deps exit)



-- VERIFY APP


verifyApp :: AppInfo -> Task.Task (Map Name Version)
verifyApp info =
  if _app_elm_version info /= Compiler.version then
    throw (E.AppBadElm (_app_elm_version info))

  else
    do  oldSolution <- appToSolution info
        let solver = Solver.solve (Map.map Con.exactly oldSolution)
        registry <- Cache.optionalUpdate
        maybeSolution <- Explorer.run registry (Solver.run solver)
        case maybeSolution of
          Nothing ->
            throw E.BadDeps

          Just newSolution ->
            if Map.size oldSolution == Map.size newSolution then
              return newSolution
            else
              throw (E.AppMissingTrans (Map.toList (Map.difference newSolution oldSolution)))



-- VERIFY PKG


verifyPkg :: PkgInfo -> Task.Task (Map Name Version)
verifyPkg info =
  if not (Con.goodElm (_pkg_elm_version info)) then
    throw (E.PkgBadElm (_pkg_elm_version info))

  else
    do  deps <- union noDups (_pkg_deps info) (_pkg_test_deps info)
        let solver = Solver.solve deps
        registry <- Cache.optionalUpdate
        maybeSolution <- Explorer.run registry (Solver.run solver)
        case maybeSolution of
          Nothing ->
            throw E.BadDeps

          Just solution ->
            return solution



-- APP TO SOLUTION


appToSolution :: Project.AppInfo -> Task.Task (Map Name Version)
appToSolution (Project.AppInfo _ _ depsDirect depsTrans testDirect testTrans) =
  do  a <- union allowEqualDups depsTrans testDirect
      b <- union noDups depsDirect testTrans
      union noDups a b


noDups :: Name -> a -> a -> Task.Task a
noDups _ _ _ =
  throw E.BadDeps


allowEqualDups :: Name -> Version -> Version -> Task.Task Version
allowEqualDups _ v1 v2 =
  if v1 == v2 then
    return v1
  else
    throw E.BadDeps


union :: (Name -> a -> a -> Task.Task a) -> Map Name a -> Map Name a -> Task.Task (Map Name a)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2



-- VERIFY ARTIFACTS


verifyArtifacts :: Map Name Version -> Task.Task RawInfo
verifyArtifacts solution =
  do  Website.download =<< filterM noSrc (Map.toList solution)
      verifyBuildArtifacts solution


noSrc :: (Name, Version) -> Task.Task Bool
noSrc (name, version) =
  do  root <- Task.getPackageCacheDirFor name version
      liftIO $ not <$> doesDirectoryExist (root </> "src")



-- VERIFY BUILD ARTIFACTS


verifyBuildArtifacts :: Map Name Version -> Task.Task RawInfo
verifyBuildArtifacts solution =
  do  Task.report (Progress.BuildDepsStart (Map.size solution))
      startMVar <- liftIO newEmptyMVar
      ifacesMVar <- liftIO $ newMVar Map.empty
      pkgInfoMVars <- Map.traverseWithKey (verifyBuild startMVar ifacesMVar) solution
      liftIO $ putMVar startMVar pkgInfoMVars
      answers <- liftIO $ traverse readMVar pkgInfoMVars
      Task.report Progress.BuildDepsEnd
      RawInfo
        <$> Map.traverseMaybeWithKey toInfo answers
        <*> liftIO (readMVar ifacesMVar)


data RawInfo =
  RawInfo (Map Name PkgInfo) Module.Interfaces


verifyBuild
  :: MVar (Map Name (MVar Answer))
  -> MVar Module.Interfaces
  -> Name
  -> Version
  -> Task.Task (MVar Answer)
verifyBuild pkgInfoMVar ifacesMVar name version =
  do  mvar <- liftIO newEmptyMVar
      info <- Cache.getElmJson name version
      report <- Task.getReporter
      runner <- Task.getSilentRunner

      liftIO $ void $ forkIO $
        do  allMVars <- readMVar pkgInfoMVar
            let deps = Project._pkg_deps info
            let depsMVars = Map.intersection allMVars deps
            depAnswers <- traverse readMVar depsMVars

            answer <- ifNotBlocked depAnswers $ \infos ->
              do  ifacesBefore <- readMVar ifacesMVar
                  result <- runner (getIface name version info infos ifacesBefore)
                  case result of
                    Right ifaces ->
                      do  ifacesNow <- takeMVar ifacesMVar
                          putMVar ifacesMVar (Map.union ifacesNow ifaces)
                          return (Ok info)

                    Left _ ->
                      return (Err name version)

            report Progress.BuildDepsProgress
            putMVar mvar answer

      return mvar



-- ANSWERS


data Answer
  = Ok PkgInfo
  | Blocked
  | Err Name Version


toInfo :: Name -> Answer -> Task.Task (Maybe PkgInfo)
toInfo _ answer =
  case answer of
    Ok info ->
      return (Just info)

    Blocked ->
      return Nothing

    Err name version ->
      do  elmHome <- liftIO PerUserCache.getElmHome
          throw (E.BuildFailure elmHome name version)


ifNotBlocked :: Map Name Answer -> (Map Name PkgInfo -> IO Answer) -> IO Answer
ifNotBlocked answers callback =
  case traverse isOk answers of
    Nothing ->
      return Blocked

    Just infos ->
      callback infos


isOk :: Answer -> Maybe PkgInfo
isOk answer =
  case answer of
    Ok info -> Just info
    Blocked -> Nothing
    Err _ _ -> Nothing



-- GET INTERFACE


getIface
  :: Name
  -> Version
  -> PkgInfo
  -> Map Name PkgInfo
  -> Module.Interfaces
  -> Task.Task Module.Interfaces
getIface name version info infos depIfaces =
  do  root <- Task.getPackageCacheDirFor name version
      let solution = Map.map _pkg_version infos

      cached <- isCached root solution

      if cached
        then IO.readBinary (root </> "ifaces.dat")
        else
          do  Paths.removeStuff root
              let docsPath = root </> "docs.json"

              let summary = Summary.cheapInit root info infos depIfaces
              args <- Args.fromSummary summary
              graph <- Crawl.crawl summary args
              (dirty, cachedIfaces) <- Plan.plan (Just docsPath) summary graph
              answers <- Compile.compile (Pkg info) (Just docsPath) cachedIfaces dirty
              results <- Artifacts.ignore answers
              _ <- Artifacts.writeDocs results docsPath

              Paths.removeStuff root

              updateCache root name info solution graph results



-- IS CACHED?


isCached :: FilePath -> Map Name Version -> Task.Task Bool
isCached root solution =
  IO.andM
    [ IO.exists (root </> "cached.dat")
    , IO.exists (root </> "ifaces.dat")
    , IO.exists (root </> "objs.dat")
    , IO.exists (root </> "documentation.json")
    , isCachedHelp solution <$> IO.readBinary (root </> "cached.dat")
    ]


isCachedHelp :: Map Name Version -> Map Name (Set Version) -> Bool
isCachedHelp solution cachedDeps =
  let
    matches =
      Map.intersectionWith Set.member solution cachedDeps
  in
    Map.size solution == Map.size matches
    && Map.foldr (&&) True matches



-- UPDATE CACHE


updateCache
  :: FilePath
  -> Name
  -> PkgInfo
  -> Map Name Version
  -> Crawl.Result
  -> Map Module.Raw Compiler.Artifacts
  -> Task.Task Module.Interfaces
updateCache root name info solution graph results =
  do  let path = root </> "cached.dat"
      let deps = Map.map Set.singleton solution
      let elmi = crush name info results

      exists <- IO.exists path

      if exists
        then
          do  oldDeps <- IO.readBinary path
              IO.writeBinary path (Map.unionWith Set.union deps oldDeps)
        else
          do  IO.writeBinary (root </> "ifaces.dat") elmi

              IO.writeBinary path deps

              IO.writeBinary (root </> "objs.dat") $
                Map.foldr addGraph (objGraphFromKernels graph) results

              liftIO $ mapM_ (\(name, hs) -> do
                let path = Paths.haskelmoWithoutStuff root name
                createDirectoryIfMissing True (takeDirectory path)
                writeFile path (HsPretty.prettyPrint hs)) $ Map.toList $ Compiler._haskelmo <$> results


              liftIO $ Encode.write (root </> "documentation.json") $
                Encode.list Docs.encode $
                  Map.foldr addDocs [] results

              -- generate package.yaml from PkgInfo (a.k.a. package-verison of elm.json)
              liftIO $ encodeFile (Paths.haskellPackageYaml root) (packageYamlFromPkgInfo info)


      return elmi


-- HASKELM PROJECT FILE GENERATION

{-
data PkgInfo =
  PkgInfo
    { _pkg_name :: Name
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Version
    , _pkg_exposed :: Exposed
    , _pkg_deps :: Map Name Con.Constraint
    , _pkg_test_deps :: Map Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    }
-}

data HPackYaml =
  HPackYaml
  { name :: Text
  , synopsis :: Text
  , license :: Text
  , version :: Text
  , exposedModules :: List Text
  , dependencies :: List Text
  }


instance ToJSON HPackYaml where
  toJSON (HPackYaml
    name
    synopsis
    license
    version
    exposedModules
    dependencies)
    = object
      [ "name" .= name
      , "synopsis" .= synopsis
      , "license" .= license
      , "version" .= version
      , "dependencies" .= array (Aeson.String <$> dependencies)
      -- hard-coded values
      , "default-extensions" .= array (Aeson.String <$>
        [ "ConstraintKinds"
        , "TypeFamilies"
        , "TypeOperators"
        , "OverloadedStrings"
        , "OverloadedLabels"
        , "DataKinds"
        , "NoImplicitPrelude"
        , "DeriveAnyClass"
        , "NoMonomorphismRestriction"
        , "GADTs"
        , "StandaloneDeriving"
        , "FlexibleContexts" -- This prevents issues with SuperRecord when it's used kinda anonymously (i.e. passed around a lot between function calls in a single function body)
        ])
      , "ghc-options" .= Aeson.String "-Wall -Werror"
      , "library" .= object
        [ "exposed-modules" .= array (Aeson.String <$> exposedModules)
        , "source-dirs" .= array [Aeson.String "src"]
        ]
      ]


{-
    "type": "package",
    "name": "elm/url",
    "summary": "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Url",
        "Url.Builder",
        "Url.Parser",
        "Url.Parser.Query"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {
    }
-}

packageYamlFromPkgInfo :: PkgInfo -> HPackYaml
packageYamlFromPkgInfo
  info@(PkgInfo
  package
  _summary
  (License _ code)
  _version
  _exposed
  _deps
  _test_deps
  _elm_version) =
  let
    name = Paths.cabalNameOfPackage package
    synopsis = _summary
    license = code
    version = Pkg.versionToText _version
    exposedModules = N.toText <$> ElmJson.getExposed info
    dependencies =
      fmap (\(fqname, constraint) -> Paths.cabalNameOfPackage fqname <> " " <> constraint)
      $ Map.toList
      $ convertConstraints
        <$> _deps
  in
  HPackYaml
    name
    synopsis
    license
    version
    exposedModules
    dependencies

convertConstraints :: Con.Constraint -> Text
convertConstraints (Con.Range vlower op1 op2 vupper) =
  -- vlower op1 "v" op2 vupper
  -- 1.2.3  <=   v   <  1.3.1
  let
    opToStr (Con.Less) = "<"
    opToStr (Con.LessOrEqual) = "<="
    revOpToStr (Con.Less) = ">"
    revOpToStr (Con.LessOrEqual) = ">="
  in
    revOpToStr op1
      <> " "
      <> Pkg.versionToText vlower
      <> " && "
      <> opToStr op2
      <> " "
      <> Pkg.versionToText vupper



-- CRUSH INTERFACES


crush :: Name -> PkgInfo -> Map Module.Raw Compiler.Artifacts -> Module.Interfaces
crush pkg info results =
  let
    exposed =
      Set.fromList (Project.getExposed info)
  in
    Map.mapKeys (Module.Canonical pkg) $
      Map.mapMaybeWithKey (crushHelp exposed) results


crushHelp :: Set Module.Raw -> Module.Raw -> Compiler.Artifacts -> Maybe Module.Interface
crushHelp exposed name (Compiler.Artifacts elmi _ _ _) =
  if Set.member name exposed then
    Just elmi

  else
    Nothing



-- DOCUMENTATION


addDocs :: Compiler.Artifacts -> [Docs.Module] -> [Docs.Module]
addDocs (Compiler.Artifacts _ _ _ maybeDocs) docsList =
  case maybeDocs of
    Nothing ->
      docsList

    Just docs ->
      docs : docsList



-- OBJECT GRAPH


addGraph :: Compiler.Artifacts -> Obj.Graph -> Obj.Graph
addGraph (Compiler.Artifacts _ elmo _ _) graph =
  Obj.union elmo graph


objGraphFromKernels :: Crawl.Result -> Obj.Graph
objGraphFromKernels (Crawl.Graph _ _ kernels _ _) =
  Obj.fromKernels kernels
