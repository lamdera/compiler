{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings,
TemplateHaskell
#-}
module Elm.Details
  ( Details(..)
  , BuildID, zero
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , load
  , loadObjects
  , loadInterfaces
  , verifyInstall
  --
  , eDetails, dDetails
  --
  , isCompatibleAppElmVersion -- @LAMDERA exposed for Test.MultiVersion
  )
  where


import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Coerce (coerce)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word64)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash
import qualified ThreadSafe.Fork as Fork

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified Compile
import qualified Deps.Registry as Registry
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Constraint as Con
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.Kernel as Kernel
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R


import qualified Lamdera
import Lamdera ((&))
import qualified Lamdera.Extensions
import qualified Lamdera.PackageReplacements
import qualified Lamdera.Version
import qualified Type.Type -- @LAMDERA withSharedTables

-- DETAILS


data Details =
  Details
    { _outlineTime :: File.Time
    , _outline :: ValidOutline
    , _buildID :: BuildID
    , _locals :: Map.Map Module.Name Local
    , _foreigns :: Map.Map Module.Name Foreign
    , _extras :: Extras
    }


newtype BuildID = BuildID Word64
  deriving (Eq, Ord)


zero :: BuildID
zero =
  BuildID 0


data ValidOutline
  = ValidApp (NE.List R.Path)
  | ValidPkg Pkg.Name [Module.Name] (Map.Map Pkg.Name V.Version {- for docs in reactor -})


-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--
data Local =
  Local
    { _path :: FilePath
    , _time :: File.Time
    , _deps :: [Module.Name]
    , _main :: Bool
    , _lastChange :: BuildID
    , _lastCompile :: BuildID
    }


data Foreign =
  Foreign Pkg.Name [Pkg.Name]


data Extras
  = ArtifactsCached
  | ArtifactsFresh Interfaces Opt.GlobalGraph


type Interfaces =
  Map.Map ModuleName.Canonical I.DependencyInterface



-- LOAD ARTIFACTS


loadObjects :: R.Stuff -> Details -> IO (Fork.SafeMVar (Maybe Opt.GlobalGraph))
loadObjects stuff (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh _ o -> Fork.cached $ Just o
    ArtifactsCached    -> Fork.fork_  $ File.readBytes Opt.dGlobalGraph (R.objects stuff)


loadInterfaces :: R.Stuff -> Details -> IO (Fork.SafeMVar (Maybe Interfaces))
loadInterfaces stuff (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh i _ -> Fork.cached $ Just i
    ArtifactsCached    -> Fork.fork_  $ File.readBytes (D.dict32 ModuleName.dCanonical I.dDependencyInterface) (R.interfaces stuff)



-- VERIFY INSTALL -- used by Install


verifyInstall :: File.Writer R.PROJECT -> R.Root -> R.Stuff -> Solver.Env -> Outline.Outline -> IO (Either Exit.Details ())
verifyInstall writer root stuff (Solver.Env cache manager connection registry) outline =
  do  time <- File.getTime (R.elm_json root)
      let key = Reporting.ignorer
      let env = Env key stuff cache manager connection registry
      case outline of
        Outline.Pkg pkg -> Task.run (verifyPkg writer env time pkg >> return ())
        Outline.App app -> Task.run (verifyApp writer env time app >> return ())



-- LOAD -- used by Make, Repl, Reactor


load :: File.Writer R.PROJECT -> Reporting.Style -> R.Root -> R.Stuff -> IO (Either Exit.Details Details)
load writer style root stuff =
  do  newTime <- File.getTime (R.elm_json root)
      maybeDetails <- File.readBytes dDetails (R.details stuff)
      case maybeDetails of
        Nothing ->
          generate writer style root stuff newTime

        Just details@(Details oldTime _ (BuildID n) _ _ _) ->
          if oldTime == newTime
          then return (Right details { _buildID = BuildID (n + 1) })
          else generate writer style root stuff newTime



-- GENERATE


generate :: File.Writer R.PROJECT -> Reporting.Style -> R.Root -> R.Stuff -> File.Time -> IO (Either Exit.Details Details)
generate writer style root stuff time =
  Reporting.trackDetails style $ \key ->
    do  -- @LAMDERA an app first tries the cached registry (see generateFromCachedRegistry)
        cachedResult <- generateFromCachedRegistry writer key root stuff time
        case cachedResult of
          Just details -> return details
          Nothing -> generateOnline writer key root stuff time


generateOnline :: File.Writer R.PROJECT -> Reporting.DKey -> R.Root -> R.Stuff -> File.Time -> IO (Either Exit.Details Details)
generateOnline writer key root stuff time =
    do  result <- initEnv key root stuff
        case result of
          Left exit ->
            return (Left exit)

          Right (env, outline) ->
            case outline of
              Outline.Pkg pkg -> Task.run (verifyPkg writer env time pkg)
              Outline.App app -> Task.run (verifyApp writer env time app)


{-| @LAMDERA Building details asks the package server for registry updates first
(Solver.initEnv), a network round trip on every build without elm-stuff that took
longer than compiling a whole small app. An application's elm.json pins exact
versions, so the solver only has to find those versions: if the registry as last
fetched has them, the solution is the one a fresh registry gives. So an app tries
the cached registry first, and only when that finds no solution (a pinned version
newer than the cached registry) does it go through the usual path, which updates the
registry and solves again. Packages (version ranges, where a fresh registry can give
a newer solution) always take the usual path, as do Install, Init and friends.
Downloading missing packages and their elm.json works as before either way.
-}
generateFromCachedRegistry :: File.Writer R.PROJECT -> Reporting.DKey -> R.Root -> R.Stuff -> File.Time -> IO (Maybe (Either Exit.Details Details))
generateFromCachedRegistry writer key root stuff time =
  do  eitherOutline <- Outline.read root False
      case eitherOutline of
        Right (Outline.App app) ->
          do  maybeEnv <- Solver.initEnvCached
              case maybeEnv of
                Nothing ->
                  return Nothing

                Just (Solver.Env cache manager connection registry) ->
                  do  let env = Env key stuff cache manager connection registry
                      result <- Task.run (verifyApp writer env time app)
                      case result of
                        Left (Exit.DetailsNoSolution _) -> return Nothing
                        _ -> return (Just result)

        _ ->
          return Nothing



-- ENV


data Env =
  Env
    { _key :: Reporting.DKey
    , _stuff :: R.Stuff
    , _cache :: R.PackageCache
    , _manager :: Http.Manager
    , _connection :: Solver.Connection
    , _registry :: Registry.Registry
    }


initEnv :: Reporting.DKey -> R.Root -> R.Stuff -> IO (Either Exit.Details (Env, Outline.Outline))
initEnv key root stuff =
  do  mvar <- Fork.fork_ Solver.initEnv
      eitherOutline <- Outline.read root False
      case eitherOutline of
        Left problem ->
          return $ Left $ Exit.DetailsBadOutline problem

        Right outline ->
          do  maybeEnv <- Fork.await mvar
              case maybeEnv of
                Left problem ->
                  return $ Left $ Exit.DetailsCannotGetRegistry problem

                Right (Solver.Env cache manager connection registry) ->
                  return $ Right (Env key stuff cache manager connection registry, outline)



-- VERIFY PROJECT


type Task a = Task.Task Exit.Details a


verifyPkg :: File.Writer R.PROJECT -> Env -> File.Time -> Outline.PkgOutline -> Task Details
verifyPkg writer env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
  if Con.goodElm elm
  then
    do  solution <- verifyConstraints env =<< union noDups direct testDirect
        let exposedList = Outline.flattenExposed exposed
        let exactDeps = Map.map (\(Solver.Details v _) -> v) solution -- for pkg docs in reactor
        verifyDependencies writer env time (ValidPkg pkg exposedList exactDeps) solution direct
  else
    Task.throw $ Exit.DetailsBadElmInPkg elm


-- @LAMDERA A Lamdera compiler built on elm 0.19.2 still accepts apps whose elm.json
-- declares an older 0.19.x (0.19.1) - and any 0.19.x up to the compiler's own version -
-- so existing deployed apps build unmodified without a forced elm.json bump. 0.19.1 and
-- 0.19.2 are language/compiler compatible; and caches can't go stale across the bump
-- because every cache path is keyed by the elm version (elm-stuff/lamdera-<ver>-<elm>/,
-- Stuff.stuff), so 0.19.2 writes fresh caches and never reads 0.19.1's Region-format data.
--
-- On elm 0.19.3 the same holds: apps declaring 0.19.1, 0.19.2 or 0.19.3 all build, and the
-- caches are keyed by 0.19.3 (elm-stuff/lamdera-<ver>-0.19.3/, Root.getStuff).
isCompatibleAppElmVersion :: V.Version -> Bool
isCompatibleAppElmVersion elmVersion =
  V.fromVersion elmVersion $ \major minor _ ->
    major == 0 && minor == 19 && elmVersion <= V.compiler


verifyApp :: File.Writer R.PROJECT -> Env -> File.Time -> Outline.AppOutline -> Task Details
verifyApp writer env time outline@(Outline.AppOutline elmVersion srcDirs direct _ _ _) =
  if isCompatibleAppElmVersion elmVersion -- @LAMDERA was: elmVersion == V.compiler
  then
    do  stated <- checkAppDeps outline
        actual <- verifyConstraints env (Map.map Con.exactly stated)
        if Map.size stated == Map.size actual
          then verifyDependencies writer env time (ValidApp srcDirs) actual direct
          else Task.throw $ Exit.DetailsHandEditedDependencies
            $ let
              stateds = stated & fmap (\k _ -> k)
              actuals = actual & fmap (\k _ -> k)

              -- first, everything in stated that's not in actual
              missing = Map.difference stateds actuals & Map.toList & fmap fst & show
              -- then, everything in actual that's not in stated
              extra = Map.difference actuals stateds & Map.toList & fmap fst & show
            in
            "The stated and actual dependencies do not match.\n\n"
            ++ "Stated missing in actual: " ++ show missing
            ++ "\n\nActual missing in stated: " ++ show extra
  else
    Task.throw $ Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps :: Outline.AppOutline -> Task (Map.Map Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  do  x <- union allowEqualDups indirect testDirect
      y <- union noDups direct testIndirect
      union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints :: Env -> Map.Map Pkg.Name Con.Constraint -> Task (Map.Map Pkg.Name Solver.Details)
verifyConstraints (Env _ _ cache _ connection registry) constraints =
  do  result <- Task.io $ Solver.verify cache connection registry constraints
      case result of
        Solver.Ok details        -> return details
        Solver.NoSolution        -> Task.throw $ Exit.DetailsNoSolution constraints
        Solver.NoOfflineSolution -> Task.throw $ Exit.DetailsNoOfflineSolution
        Solver.Err exit          -> Task.throw $ Exit.DetailsSolverProblem exit



-- UNION


union :: (Ord k) => (k -> v -> v -> Task v) -> Map.Map k v -> Map.Map k v -> Task (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2


noDups :: k -> v -> v -> Task v
noDups _ _ _ =
  Task.throw $ Exit.DetailsHandEditedDependencies
    "The same dependency is listed in multiple places in your elm.json file."


allowEqualDups :: (Eq v, Show v) => k -> v -> v -> Task v
allowEqualDups _ v1 v2 =
  if v1 == v2
  then return v1
  else Task.throw $ Exit.DetailsHandEditedDependencies
    $ "The same dependency is listed in multiple places in your elm.json file, but with different constraints.\n\n"
    ++ "Dep 1: " ++ show v1 ++ "\n\nDep 2: " ++ show v2



-- VERIFY DEPENDENCIES


verifyDependencies :: File.Writer R.PROJECT -> Env -> File.Time -> ValidOutline -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task Details
verifyDependencies writer env@(Env key stuff cache _ _ _) time outline solution directDeps =
  Task.eio id $
  do  Reporting.report key (Reporting.DStart (Map.size solution))
      mvar <- newEmptyMVar
      mvars <- R.withRegistryLock cache $ \pkg_writer ->
        Fork.forkWithKey (verifyDep pkg_writer env mvar solution) solution
      putMVar mvar mvars
      deps <- traverse Fork.await mvars
      case sequence deps of
        Left _ ->
          do  home <- R.getElmHome
              return $ Left $ Exit.DetailsBadDeps home $
                Maybe.catMaybes $ Either.lefts $ Map.elems deps

        Right artifacts ->
          let
            objs = Map.foldr addObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
            details = Details time outline zero Map.empty foreigns (ArtifactsFresh ifaces objs)
          in
          do  File.writeBytes_ writer (R.objects    stuff) Opt.eGlobalGraph objs
              File.writeBytes_ writer (R.interfaces stuff) (E.dict32 ModuleName.eCanonical I.eDependencyInterface) ifaces
              File.writeBytes_ writer (R.details    stuff) eDetails details
              return (Right details)


addObjects :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph


addInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $ Map.mapKeysMonotonic (ModuleName.Canonical pkg) $
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map Module.Name (OneOrMore.OneOrMore Pkg.Name) -> Map.Map Module.Name (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _      -> Just (OneOrMore.one pkg)
        I.Private _ _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map Module.Name I.DependencyInterface
    , _objects :: Opt.GlobalGraph
    }
  deriving (Show)


type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep :: File.Writer R.PACKAGES -> Env -> MVar (Map.Map Pkg.Name (Fork.SafeMVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Dep
verifyDep writer (Env key _ cache manager _ _) depsMVar solution pkg details@(Solver.Details vsn directDeps) =
  do  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
      exists <- Dir.doesDirectoryExist (R.package cache pkg vsn </> "src")
      if exists
        then
          do  Reporting.report key Reporting.DCached
              maybeCache <- File.readBytes dArtifactCache (R.package cache pkg vsn </> "artifacts.dat")
                              & Lamdera.alternativeImplementation (File.readBytes dArtifactCache (R.package cache pkg vsn </> Lamdera.Version.artifacts))
              case maybeCache of
                Nothing ->
                  build writer key cache depsMVar pkg details fingerprint Set.empty

                Just (ArtifactCache fingerprints artifacts) ->
                  if Set.member fingerprint fingerprints
                    then Reporting.report key Reporting.DBuilt >> return (Right artifacts)
                    else build writer key cache depsMVar pkg details fingerprint fingerprints
        else
          do  Reporting.report key Reporting.DRequested
              result <- downloadPackage cache manager pkg vsn
              case result of
                Left problem ->
                  do  Reporting.report key (Reporting.DFailed pkg vsn)
                      return $ Left $ Just $ Exit.BD_BadDownload pkg vsn problem

                Right () ->
                  do  Reporting.report key (Reporting.DReceived pkg vsn)
                      build writer key cache depsMVar pkg details fingerprint Set.empty



-- ARTIFACT CACHE


data ArtifactCache =
  ArtifactCache
    { _fingerprints :: Set.Set Fingerprint
    , _artifacts :: Artifacts
    }


type Fingerprint =
  Map.Map Pkg.Name V.Version



-- BUILD


build :: File.Writer R.PACKAGES -> Reporting.DKey -> R.PackageCache -> MVar (Map.Map Pkg.Name (Fork.SafeMVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> IO Dep
build writer key cache depsMVar pkg (Solver.Details vsn _) f fs =
  Type.Type.withSharedTables $ -- @LAMDERA the type checker's shared tables last one package build
  do  eitherOutline <- Outline.read (R.packageRoot cache pkg vsn) True
      case eitherOutline of
        Left _ ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.App _) ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
          do  allDeps <- readMVar depsMVar
              directDeps <- traverse Fork.await (Map.intersection allDeps deps)
              case sequence directDeps of
                Left _ ->
                  do  Reporting.report key Reporting.DBroken
                      return $ Left $ Nothing

                Right directArtifacts ->
                  do  let src = R.package cache pkg vsn </> "src"
                      let foreignDeps = gatherForeignInterfaces directArtifacts
                      let exposedDict = Map.fromKeys (\_ -> ()) (Outline.flattenExposed exposed)
                      docsStatus <- getDocsStatus cache pkg vsn
                      mvar <- newEmptyMVar
                      mvars <- Fork.forkWithKey (crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                      putMVar mvar mvars
                      mapM_ Fork.await mvars
                      maybeStatuses <- traverse Fork.await =<< readMVar mvar
                      case sequence maybeStatuses of
                        Nothing ->
                          do  Reporting.report key Reporting.DBroken
                              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                        Just statuses ->
                          do  rmvar <- newEmptyMVar
                              rmvars <- Fork.forkWithKey (compile pkg rmvar) statuses

                              putMVar rmvar rmvars
                              maybeResults <- traverse Fork.await rmvars
                              case sequence maybeResults of
                                Nothing ->
                                  do  Reporting.report key Reporting.DBroken
                                      -- @LAMDERA the corrupt cache that lamdera reset is required for happens here.
                                      -- @TODO need to figure out a nice way to add support for revealing the compilation
                                      -- error failure from the `compile` above, which _should_ be present here, but seems
                                      -- that Elm doesn't show it because in theory it _should_ be impossible (i.e. packages
                                      -- must compile to be published)
                                      return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                                Just results ->
                                  let
                                    path = R.package cache pkg vsn </> "artifacts.dat"
                                             & Lamdera.alternativeImplementation (R.package cache pkg vsn </> Lamdera.Version.artifacts)
                                    ifaces = gatherInterfaces exposedDict results
                                    objects = gatherObjects results
                                    artifacts = Artifacts ifaces objects
                                    fingerprints = Set.insert f fs
                                  in
                                  do  writeDocs writer cache pkg vsn docsStatus results
                                      File.writeBytes writer path eArtifactCache (ArtifactCache fingerprints artifacts)
                                      Reporting.report key Reporting.DBuilt
                                      return (Right artifacts)



-- GATHER


gatherObjects :: Map.Map Module.Name Result -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results


addLocalGraph :: Module.Name -> Result -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs _ -> Opt.addLocalGraph objs graph
    RForeign _      -> graph
    RKernelLocal cs -> Opt.addKernel (Module.getKernel name) cs graph
    RKernelForeign  -> graph


gatherInterfaces :: Map.Map Module.Name () -> Map.Map Module.Name Result -> Map.Map Module.Name I.DependencyInterface
gatherInterfaces exposed artifacts =
  let
    onLeft  = Map.mapMissing (\_ -> $(Crash.crash 'gatherInterfaces) "compiler bug")
    onRight = Map.mapMaybeMissing     (\_    iface -> toLocalInterface I.private iface)
    onBoth  = Map.zipWithMaybeMatched (\_ () iface -> toLocalInterface I.public  iface)
  in
  Map.merge onLeft onRight onBoth exposed artifacts


toLocalInterface :: (I.Interface -> a) -> Result -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ _ -> Just (func iface)
    RForeign _       -> Nothing
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- GATHER FOREIGN INTERFACES


data ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface


gatherForeignInterfaces :: Map.Map Pkg.Name Artifacts -> Map.Map Module.Name ForeignInterface
gatherForeignInterfaces directArtifacts =
    Map.map (OneOrMore.destruct finalize) $
      Map.foldrWithKey gather Map.empty directArtifacts
  where
    finalize :: I.Interface -> [I.Interface] -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _:_ -> ForeignAmbiguous

    gather :: Pkg.Name -> Artifacts -> Map.Map Module.Name (OneOrMore.OneOrMore I.Interface) -> Map.Map Module.Name (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic :: I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface  -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing



-- CRAWL


type StatusDict =
  Map.Map Module.Name (Fork.SafeMVar (Maybe Status))


data Status
  = SLocal DocsStatus (Map.Map Module.Name ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal [Kernel.Chunk]
  | SKernelForeign


crawlModule :: Map.Map Module.Name ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> Module.Name -> () -> IO (Maybe Status)
crawlModule foreignDeps mvar pkg src docsStatus name () =
  do  let path = src </> Module.toFilePath name <.> "elm"
      exists <- File.exists path
      case Map.lookup name foreignDeps of
        Just ForeignAmbiguous ->
          return Nothing

        Just (ForeignSpecific iface) ->
          if exists
          then return Nothing
          else return (Just (SForeign iface))

        Nothing ->
          if exists then
            crawlFile foreignDeps mvar pkg src docsStatus name path

          else if Pkg.isKernel pkg && Module.isKernel name then
            crawlKernel foreignDeps mvar pkg src name

          else
            return Nothing


crawlFile :: Map.Map Module.Name ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> Module.Name -> FilePath -> IO (Maybe Status)
crawlFile foreignDeps mvar pkg src docsStatus expectedName path =
  do  bytes <- File.readUtf8 path
               & Lamdera.alternativeImplementationPassthrough (Lamdera.PackageReplacements.getReplacement pkg expectedName)
      result <- Parse.fromByteString (Parse.Package pkg) bytes
      case result of
        Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) | expectedName == actualName ->
          do  deps <- crawlImports foreignDeps mvar pkg src imports
              return (Just (SLocal docsStatus deps modul))

        _ ->
          return Nothing


crawlImports :: Map.Map Module.Name ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> [Src.Import] -> IO (Map.Map Module.Name ())
crawlImports foreignDeps mvar pkg src imports =
  do  statusDict <- takeMVar mvar
      let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
      let news = Map.difference deps statusDict
      mvars <- Fork.forkWithKey (crawlModule foreignDeps mvar pkg src DocsNotNeeded) news
      putMVar mvar (Map.union mvars statusDict)
      mapM_ Fork.await mvars
      return deps


crawlKernel :: Map.Map Module.Name ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> Module.Name -> IO (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
  do  let path = src </> Module.toFilePath name <.> "js"
      exists <- File.exists path
      if exists
        then
          do  bytes <- File.readUtf8 path
                       & Lamdera.alternativeImplementationPassthrough (Lamdera.PackageReplacements.getReplacement pkg name)
              result <- Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes
              case result of
                Nothing ->
                  return Nothing

                Just (Kernel.Content imports chunks) ->
                  do  _ <- crawlImports foreignDeps mvar pkg src imports
                      return (Just (SKernelLocal chunks))
        else
          return (Just SKernelForeign)


getDepHome :: ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous                          -> Nothing



-- COMPILE


data Result
  = RLocal !I.Interface !Opt.LocalGraph (Maybe Docs.Module)
  | RForeign I.Interface
  | RKernelLocal [Kernel.Chunk]
  | RKernelForeign


compile :: Pkg.Name -> MVar (Map.Map Module.Name (Fork.SafeMVar (Maybe Result))) -> Module.Name -> Status -> IO (Maybe Result)
compile pkg mvar _ status =
  case status of
    SLocal docsStatus deps modul ->
      do  resultsDict <- readMVar mvar
          maybeResults <- traverse Fork.await (Map.intersection resultsDict deps)
          case sequence maybeResults of
            Nothing ->
              return Nothing

            Just results ->
              case Compile.compile Nothing pkg (Map.mapMaybe getInterface results) modul of
                Left _ ->
                  return Nothing

                Right (Compile.Artifacts canonical annotations objects) ->
                  do  let ifaces = I.fromModule pkg canonical annotations
                      docs <- makeDocs docsStatus canonical
                      return $ Just $ RLocal ifaces objects docs

    SForeign iface ->
      return (Just (RForeign iface))

    SKernelLocal chunks ->
      return (Just (RKernelLocal chunks))

    SKernelForeign ->
      return (Just RKernelForeign)


getInterface :: Result -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ _ -> Just iface
    RForeign iface   -> Just iface
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- MAKE DOCS


data DocsStatus
  = DocsNeeded
  | DocsNotNeeded


getDocsStatus :: R.PackageCache -> Pkg.Name -> V.Version -> IO DocsStatus
getDocsStatus cache pkg vsn =
  do  exists <- File.exists (R.package cache pkg vsn </> "docs.json")
      if exists
        then return DocsNotNeeded
        else return DocsNeeded


makeDocs :: DocsStatus -> Can.Module -> IO (Maybe Docs.Module)
makeDocs status modul =
  case status of
    DocsNeeded ->
      do  result <- Docs.fromModule modul
          case result of
            Right docs -> return $ Just docs
            Left _     -> return $ Nothing

    DocsNotNeeded ->
      return Nothing


writeDocs :: File.Writer R.PACKAGES -> R.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Map.Map Module.Name Result -> IO ()
writeDocs writer cache pkg vsn status results =
  case status of
    DocsNeeded ->
      JE.writeUgly writer (R.package cache pkg vsn </> "docs.json") $
        Docs.encode $ Map.mapMaybe toDocs results

    DocsNotNeeded ->
      return ()


toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RLocal _ _ docs -> docs
    RForeign _      -> Nothing
    RKernelLocal _  -> Nothing
    RKernelForeign  -> Nothing



-- DOWNLOAD PACKAGE


downloadPackage :: R.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> IO (Either Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
  let
    url = Website.metadata pkg vsn "endpoint.json"
  in
  do  eitherByteString <-
        Http.get manager url [] id (return . Right)
          & Lamdera.alternativeImplementationPassthrough (Lamdera.Extensions.endpointJsonOverride pkg vsn)

      pkgsPath <- Lamdera.getLamderaPkgPath -- @LAMDERA

      case eitherByteString of
        Left err ->
          return $ Left $ Exit.PP_BadEndpointRequest err

        Right byteString ->
          do  result <- JD.fromByteString endpointDecoder byteString
              case result of
                Left _ ->
                  return $ Left $ Exit.PP_BadEndpointContent url

                Right (endpoint, expectedHash) ->
                  do  result <-
                        Http.getArchive manager endpoint (\e -> Exit.PP_BadArchiveRequest e Nothing) (Exit.PP_BadArchiveContent endpoint) $
                          \(sha, archive) ->
                            if expectedHash == Http.shaToChars sha
                              || pkgsPath /= Nothing -- @LAMDERA when we're debugging with a local packages path, skip hash checking
                            then Right <$> Http.writePackage (R.package cache pkg vsn) archive
                            else return $ Left $ Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha) Nothing

                      -- @LAMDERA both of those failures are usually a GitHub rename, so go find
                      -- out where the project actually went and say so in the error.
                      case result of
                        Right a -> return (Right a)
                        Left problem -> Left <$> attachMoveInfo manager pkg problem


-- @LAMDERA a published Elm version is immutable, but the GitHub repo serving it is not.
-- When a download 404s or fails its hash check, follow the repo's redirect to see if the
-- author renamed themselves, and check whether they re-published under the new name, so
-- the error can name the new location instead of leaving the user to guess.
--
-- Only ever runs on an already-failing path, and degrades to the old, less specific
-- message if any of these probes fail.
attachMoveInfo :: Http.Manager -> Pkg.Name -> Exit.PackageProblem -> IO Exit.PackageProblem
attachMoveInfo manager pkg problem =
  let
    withMoved rebuild =
      do  maybeMoved <- detectMove manager pkg
          return (rebuild maybeMoved)
  in
  case problem of
    Exit.PP_BadArchiveRequest err Nothing ->
      withMoved (Exit.PP_BadArchiveRequest err)

    Exit.PP_BadArchiveHash url expected actual Nothing ->
      withMoved (Exit.PP_BadArchiveHash url expected actual)

    _ ->
      return problem


detectMove :: Http.Manager -> Pkg.Name -> IO (Maybe Exit.PackageMoved)
detectMove manager pkg =
  do  maybeTarget <- Http.getRedirectTarget manager ("https://github.com/" ++ Pkg.toChars pkg)
      case maybeTarget >>= githubRepoName of
        Nothing ->
          return Nothing

        Just newName ->
          if newName == Pkg.toChars pkg
            then return Nothing
            else
              do  published <- isPublished manager newName
                  return $ Just $ Exit.PackageMoved newName published


githubRepoName :: String -> Maybe String
githubRepoName url =
  let
    stripped = List.stripPrefix "https://github.com/" url
  in
  case fmap (splitOn '/') stripped of
    Just [author, project] | not (null author) && not (null project) ->
      Just (author ++ "/" ++ dropSuffix ".git" project)

    _ ->
      Nothing


splitOn :: Char -> String -> [String]
splitOn c s =
  case break (== c) s of
    (before, []) -> [before]
    (before, _:rest) -> before : splitOn c rest


dropSuffix :: String -> String -> String
dropSuffix suffix s =
  if suffix `List.isSuffixOf` s
    then take (length s - length suffix) s
    else s


isPublished :: Http.Manager -> String -> IO Bool
isPublished manager name =
  do  result <-
        Http.get manager ("https://package.elm-lang.org/packages/" ++ name ++ "/releases.json") []
          (\_ -> ()) (\_ -> return (Right ()))
      return (result == Right ())


endpointDecoder :: JD.Decoder e (String, String)
endpointDecoder =
  do  url <- JD.field "url" JD.jsonString
      hash <- JD.field "hash" JD.jsonString
      return (Utf8.toChars url, Utf8.toChars hash)



-- BINARY


eDetails :: Details -> E.Builder
eDetails (Details t o i l f _) =
  File.eTime t
  <> eValidOutline o
  <> eBuildID i
  <> E.dict32 Module.eName eLocal l
  <> E.dict32 Module.eName eForeign f


dDetails :: D.Decoder Details
dDetails =
  do  t <- File.dTime
      o <- dValidOutline
      i <- dBuildID
      l <- D.dict32 Module.dName dLocal
      f <- D.dict32 Module.dName dForeign
      return (Details t o i l f ArtifactsCached)


eValidOutline :: ValidOutline -> E.Builder
eValidOutline outline =
  case outline of
    ValidApp s     -> E.u8# 0#Word8 <> NE.eList32 Outline.ePath s
    ValidPkg p m d -> E.u8# 1#Word8 <> Pkg.eName p <> E.list32 Module.eName m <> E.dict32 Pkg.eName V.eVersion d


dValidOutline :: D.Decoder ValidOutline
dValidOutline =
  do  tag <- D.u8
      case tag of
        0 -> liftM  ValidApp (NE.dList32 Outline.dPath)
        1 -> liftM3 ValidPkg Pkg.dName (D.list32 Module.dName) (D.dict32 Pkg.dName V.dVersion)
        _ -> D.expecting "ValidOutline"


eBuildID :: BuildID -> E.Builder
eBuildID (BuildID n) =
  E.u64 n


dBuildID :: D.Decoder BuildID
dBuildID =
  coerce D.u64


eLocal :: Local -> E.Builder
eLocal (Local p t d m i j) =
  E.chars64 p <> File.eTime t <> E.list32 Module.eName d <> E.bool m <> eBuildID i <> eBuildID j


dLocal :: D.Decoder Local
dLocal =
  do  p <- D.chars64
      t <- File.dTime
      d <- D.list32 Module.dName
      m <- D.bool
      i <- dBuildID
      j <- dBuildID
      return (Local p t d m i j)


eForeign :: Foreign -> E.Builder
eForeign (Foreign p ps) =
  Pkg.eName p <> E.list32 Pkg.eName ps


dForeign :: D.Decoder Foreign
dForeign =
  liftM2 Foreign Pkg.dName (D.list32 Pkg.dName)


eArtifacts :: Artifacts -> E.Builder
eArtifacts (Artifacts i o) =
  E.dict32 Module.eName I.eDependencyInterface i <> Opt.eGlobalGraph o


dArtifacts :: D.Decoder Artifacts
dArtifacts =
  liftM2 Artifacts (D.dict32 Module.dName I.dDependencyInterface) Opt.dGlobalGraph


eArtifactCache :: ArtifactCache -> E.Builder
eArtifactCache (ArtifactCache f a) =
  E.set32 (E.dict32 Pkg.eName V.eVersion) f <> eArtifacts a


dArtifactCache :: D.Decoder ArtifactCache
dArtifactCache =
  liftM2 ArtifactCache (D.set32 (D.dict32 Pkg.dName V.dVersion)) dArtifacts


