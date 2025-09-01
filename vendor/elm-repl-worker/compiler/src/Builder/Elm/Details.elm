{- MANUALLY FORMATTED -}
module Builder.Elm.Details exposing
  ( Details(..), bDetails
  , BuildID
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , load
  , loadObjects
  , loadInterfaces
  , verifyInstall
  --
  , GlobalState
  , LocalState
  , initialState
  , lensMVInterfaces
  , lensMVGlobalGraph
  )


import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Deps.Website as Website
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Compile as Compile
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Interface as I
import Compiler.Elm.Kernel as Kernel
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.System.IO as IO
import Extra.System.MVar as MVar exposing (MVar)
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Global



-- PUBLIC STATE

type alias GlobalState d e f g h =
  Dir.GlobalState (LocalState d e f g h) d e f g h


type LocalState d e f g h = LocalState
  {- mvDep -} (MVar.State (GlobalState d e f g h) Dep)
  {- mvDepMap -} (MVar.State (GlobalState d e f g h) (Map.Map Pkg.Comparable (MVar Dep)))
  {- mvStatus -} (MVar.State (GlobalState d e f g h) (Maybe Status))
  {- mvStatusMap -} (MVar.State (GlobalState d e f g h) StatusDict)
  {- mvResult -} (MVar.State (GlobalState d e f g h) (Maybe TResult))
  {- mvResultMap -} (MVar.State (GlobalState d e f g h) (Map.Map ModuleName.Raw (MVar (Maybe TResult))))
  {- mvInterfaces -} (MVar.State (GlobalState d e f g h) (Maybe Interfaces))
  {- mvGlobalGraph -} (MVar.State (GlobalState d e f g h) (Maybe Opt.GlobalGraph))


initialState : LocalState d e f g h
initialState = LocalState
  {- mvDep -} (MVar.initialState "Dep")
  {- mvDepMap -} (MVar.initialState "DepDict")
  {- mvStatus -} (MVar.initialState "Status")
  {- mvStatusMap -} (MVar.initialState "StatusDict")
  {- mvResult -} (MVar.initialState "Result")
  {- mvResultMap -} (MVar.initialState "ResultDict")
  {- mvInterfaces -} (MVar.initialState "Interfaces")
  {- mvGlobalGraph -} (MVar.initialState "GlobalGraph")


lensMVDep : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) Dep)
lensMVDep =
  { getter = \(Global.State _ _ (LocalState x _ _ _ _ _ _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState _ bi ci di ei fi gi hi) d e f g h) -> Global.State a b (LocalState x bi ci di ei fi gi hi) d e f g h
  }

lensMVDepMap : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Map.Map Pkg.Comparable (MVar Dep)))
lensMVDepMap =
  { getter = \(Global.State _ _ (LocalState _ x _ _ _ _ _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai _ ci di ei fi gi hi) d e f g h) -> Global.State a b (LocalState ai x ci di ei fi gi hi) d e f g h
  }

lensMVStatus : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Maybe Status))
lensMVStatus =
  { getter = \(Global.State _ _ (LocalState _ _ x _ _ _ _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi _ di ei fi gi hi) d e f g h) -> Global.State a b (LocalState ai bi x di ei fi gi hi) d e f g h
  }

lensMVStatusMap : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) StatusDict)
lensMVStatusMap =
  { getter = \(Global.State _ _ (LocalState _ _ _ x _ _ _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi ci _ ei fi gi hi) d e f g h) -> Global.State a b (LocalState ai bi ci x ei fi gi hi) d e f g h
  }

lensMVResult : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Maybe TResult))
lensMVResult =
  { getter = \(Global.State _ _ (LocalState _ _ _ _ x _ _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi ci di _ fi gi hi) d e f g h) -> Global.State a b (LocalState ai bi ci di x fi gi hi) d e f g h
  }

lensMVResultMap : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Map.Map ModuleName.Raw (MVar (Maybe TResult))))
lensMVResultMap =
  { getter = \(Global.State _ _ (LocalState _ _ _ _ _ x _ _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi ci di ei _ gi hi) d e f g h) -> Global.State a b (LocalState ai bi ci di ei x gi hi) d e f g h
  }

lensMVInterfaces : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Maybe Interfaces))
lensMVInterfaces =
  { getter = \(Global.State _ _ (LocalState _ _ _ _ _ _ x _) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi ci di ei fi _ hi) d e f g h) -> Global.State a b (LocalState ai bi ci di ei fi x hi) d e f g h
  }

lensMVGlobalGraph : Lens (GlobalState d e f g h) (MVar.State (GlobalState d e f g h) (Maybe Opt.GlobalGraph))
lensMVGlobalGraph =
  { getter = \(Global.State _ _ (LocalState _ _ _ _ _ _ _ x) _ _ _ _ _) -> x
  , setter = \x (Global.State a b (LocalState ai bi ci di ei fi gi _) d e f g h) -> Global.State a b (LocalState ai bi ci di ei fi gi x) d e f g h
  }



-- PRIVATE IO


type alias IO d e f g h v =
  IO.IO (GlobalState d e f g h) v



-- LOAD ARTIFACTS


loadObjects : FilePath -> Details -> IO d e f g h (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh _ o -> MVar.new lensMVGlobalGraph (Just o)
    ArtifactsCached    -> fork lensMVGlobalGraph (\() -> File.readBinary Opt.bGlobalGraph (Stuff.objects root))


loadInterfaces : FilePath -> Details -> IO d e f g h (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh i _ -> MVar.new lensMVInterfaces (Just i)
    ArtifactsCached    -> fork lensMVInterfaces (\() -> File.readBinary bInterfaces (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall : FilePath -> Solver.Env -> Outline.Outline -> IO d e f g h (Either Exit.Details ())
verifyInstall root (Solver.Env cache manager connection registry) outline =
  IO.bind (File.getTime (Dir.addName root "elm.json")) <| \time ->
  let env = Env root cache manager connection registry in
  case outline of
    Outline.Pkg pkg -> Task.run (Task.bind (verifyPkg env time pkg) (\_ -> Task.return ()))
    Outline.App app -> Task.run (Task.bind (verifyApp env time app) (\_ -> Task.return ()))



-- DETAILS


type Details =
  Details
    {- outlineTime -} File.Time
    {- outline -} ValidOutline
    {- buildID -} BuildID
    {- locals -} (Map.Map ModuleName.Raw Local)
    {- foreigns -} (Map.Map ModuleName.Raw Foreign)
    {- extras -} Extras


setBuildID buildID (Details a b _ d e f) = Details a b buildID d e f


type alias BuildID = Int


type ValidOutline
  = ValidApp (NE.TList Outline.SrcDir)
  | ValidPkg Pkg.Name (TList ModuleName.Raw) (Map.Map Pkg.Comparable V.Version {- for docs in reactor -})


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
type Local =
  Local
    {- path -} FilePath
    {- time -} File.Time
    {- deps -} (TList ModuleName.Raw)
    {- main -} Bool
    {- lastChange -} BuildID
    {- lastCompile -} BuildID


type Foreign =
  Foreign Pkg.Name (TList Pkg.Name)


type Extras
  = ArtifactsCached
  | ArtifactsFresh Interfaces Opt.GlobalGraph


type alias Interfaces =
  Map.Map ModuleName.Comparable I.DependencyInterface



-- LOAD -- used by Make, Repl, Reactor


load : FilePath -> IO d e f g h (Either Exit.Details Details)
load root =
  IO.bind (File.getTime (Dir.addName root "elm.json")) <| \newTime ->
  IO.bind (File.readBinary bDetails (Stuff.details root)) <| \maybeDetails ->
  case maybeDetails of
    Nothing ->
      generate root newTime

    Just ((Details oldTime _ buildID _ _ _) as details) ->
      if oldTime == newTime
      then IO.return (Right (details |> setBuildID (buildID + 1)))
      else generate root newTime



-- GENERATE


generate : FilePath -> File.Time -> IO d e f g h (Either Exit.Details Details)
generate root time =
  IO.bind (initEnv root) <| \result ->
  case result of
    Left exit ->
      IO.return (Left exit)

    Right (env, outline) ->
      case outline of
        Outline.Pkg pkg -> Task.run (verifyPkg env time pkg)
        Outline.App app -> Task.run (verifyApp env time app)



-- ENV


type Env =
  Env
    {- root -} FilePath
    {- cache -} Stuff.PackageCache
    {- manager -} Http.Manager
    {- connection -} Solver.Connection
    {- registry -} Registry.Registry


initEnv : FilePath -> IO d e f g h (Either Exit.Details (Env, Outline.Outline))
initEnv root =
  IO.bind Solver.initEnv <| \maybeEnv ->
  IO.bind (Outline.read root) <| \eitherOutline ->
  case eitherOutline of
    Left problem ->
      IO.return <| Left <| Exit.DetailsBadOutline problem

    Right outline ->
      case maybeEnv of
        Left problem ->
          IO.return <| Left <| Exit.DetailsCannotGetRegistry problem

        Right (Solver.Env cache manager connection registry) ->
          IO.return <| Right (Env root cache manager connection registry, outline)



-- VERIFY PROJECT


type alias Task z d e f g h v =
  Task.Task z (GlobalState d e f g h) Exit.Details v


verifyPkg : Env -> File.Time -> Outline.PkgOutline -> Task z d e f g h Details
verifyPkg env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
  if Con.goodElm elm
  then
    Task.bind (Task.andThen (verifyConstraints env) <| union noDups direct testDirect) <| \solution ->
    let exposedList = Outline.flattenExposed exposed in
    let exactDeps = Map.map (\(Solver.Details v _) -> v) solution in -- for pkg docs in reactor
    verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
  else
    Task.throw <| Exit.DetailsBadElmInPkg elm


verifyApp : Env -> File.Time -> Outline.AppOutline -> Task z d e f g h Details
verifyApp env time ((Outline.AppOutline elmVersion srcDirs direct _ _ _) as outline) =
  if elmVersion == V.compiler
  then
    Task.bind (checkAppDeps outline) <| \stated ->
    Task.bind (verifyConstraints env (Map.map Con.exactly stated)) <| \actual ->
    if Map.size stated == Map.size actual
      then verifyDependencies env time (ValidApp srcDirs) actual direct
      else Task.throw <| Exit.DetailsHandEditedDependencies
  else
    Task.throw <| Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps : Outline.AppOutline -> Task z d e f g h (Map.Map Pkg.Comparable V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  Task.bind (union allowEqualDups indirect testDirect) <| \x ->
  Task.bind (union noDups direct testIndirect) <| \y ->
  union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints : Env -> Map.Map Pkg.Comparable Con.Constraint -> Task z d e f g h (Map.Map Pkg.Comparable Solver.Details)
verifyConstraints (Env _ cache _ connection registry) constraints =
  Task.bind (Task.io <| Solver.verify cache connection registry constraints) <| \result ->
  case result of
    Solver.Ok details        -> Task.return details
    Solver.NoSolution        -> Task.throw <| Exit.DetailsNoSolution
    Solver.NoOfflineSolution -> Task.throw <| Exit.DetailsNoOfflineSolution
    Solver.Err exit          -> Task.throw <| Exit.DetailsSolverProblem exit



-- UNION


union : (comparable -> v -> v -> Task z d e f g h v) -> Map.Map comparable v -> Map.Map comparable v -> Task z d e f g h (Map.Map comparable v)
union tieBreaker deps1 deps2 =
  Map.mergeA Task.pure Task.liftA2 (Map.preserveMissing Task.pure) (Map.preserveMissing Task.pure) (Map.zipWithAMatched Task.fmap tieBreaker) deps1 deps2


noDups : comparable -> v -> v -> Task z d e f g h v
noDups _ _ _ =
  Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups : comparable -> v -> v -> Task z d e f g h v
allowEqualDups _ v1 v2 =
  if v1 == v2
  then Task.return v1
  else Task.throw Exit.DetailsHandEditedDependencies



-- FORK


fork :
  MVar.Lens (GlobalState d e f g h) v
  -> ((() -> IO d e f g h v) -> IO d e f g h (MVar v))
fork =
  MVar.newWaiting



-- VERIFY DEPENDENCIES


verifyDependencies : Env -> File.Time -> ValidOutline -> Map.Map Pkg.Comparable Solver.Details -> Map.Map Pkg.Comparable v -> Task z d e f g h Details
verifyDependencies ((Env root _ _ _ _) as env) time outline solution directDeps =
  Task.eio identity <|
    IO.bind (MVar.newEmpty lensMVDepMap) <| \mvar ->
    IO.bind (Map.traverseWithKey IO.pure IO.liftA2 (\k v -> fork lensMVDep (verifyDep env mvar solution (Pkg.fromComparable k) v)) solution) <| \mvars ->
    IO.bind (MVar.write lensMVDepMap mvar mvars) <| \_ ->
    IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVDep) mvars) <| \deps ->
    case Map.sequenceA Either.pure Either.liftA2 deps of
      Left _ ->
        IO.bind (Stuff.getElmHome) <| \home ->
        IO.return <| Left <| Exit.DetailsBadDeps home <|
          MMaybe.catMaybes <| Either.lefts <| Map.elems deps

      Right artifacts ->
        let
          objs = Map.foldr addObjects Opt.empty artifacts
          ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
          foreigns = Map.map (OneOrMore.destruct Foreign) <| Map.foldrWithKey gatherForeigns Map.empty <| Map.intersection artifacts directDeps
          details = Details time outline 0 Map.empty foreigns (ArtifactsFresh ifaces objs)
        in
        IO.bind (File.writeBinary Opt.bGlobalGraph (Stuff.objects root) objs) <| \_ ->
        IO.bind (File.writeBinary bInterfaces (Stuff.interfaces root) ifaces) <| \_ ->
        IO.bind (File.writeBinary bDetails (Stuff.details root) details) <| \_ ->
        IO.return (Right details)


addObjects : Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph


addInterfaces : Map.Map Pkg.Comparable a -> Pkg.Comparable -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces <| Map.mapKeys (ModuleName.toComparable << ModuleName.Canonical (Pkg.fromComparable pkg)) <|
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns : Pkg.Comparable -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _      -> Just (OneOrMore.one (Pkg.fromComparable pkg))
        I.Private _ _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


type Artifacts =
  Artifacts
    {- ifaces -} (Map.Map ModuleName.Raw I.DependencyInterface)
    {- objects -} Opt.GlobalGraph


type alias Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep : Env -> MVar (Map.Map Pkg.Comparable (MVar Dep)) -> Map.Map Pkg.Comparable Solver.Details -> Pkg.Name -> Solver.Details -> () -> IO d e f g h Dep
verifyDep (Env _ cache manager _ _) depsMVar solution pkg ((Solver.Details vsn directDeps) as details) () =
  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps in
  IO.bind (Dir.doesDirectoryExist (Dir.addName (Stuff.package cache pkg vsn) "src")) <| \exists ->
  if exists
    then
      IO.bind (File.readBinary bArtifactCache (Dir.addName (Stuff.package cache pkg vsn) "artifacts.dat")) <| \maybeCache ->
      case maybeCache of
        Nothing ->
          build cache depsMVar pkg details fingerprint Set.empty

        Just (ArtifactCache fingerprints artifacts) ->
          if Set.member (toComparable fingerprint) fingerprints
            then IO.return (Right artifacts)
            else build cache depsMVar pkg details fingerprint fingerprints
    else
      IO.bind (downloadPackage cache manager pkg vsn) <| \result ->
      case result of
        Left problem ->
          IO.return <| Left <| Just <| Exit.BD_BadDownload pkg vsn problem

        Right () ->
          build cache depsMVar pkg details fingerprint Set.empty



-- ARTIFACT CACHE


type ArtifactCache =
  ArtifactCache
    {- fingerprints -} (Set.Set FingerprintComparable)
    {- artifacts -} Artifacts


type alias Fingerprint =
  Map.Map Pkg.Comparable V.Version


type alias FingerprintComparable =
  TList (Pkg.Comparable, V.Comparable)


toComparable : Fingerprint -> FingerprintComparable
toComparable f =
  Map.toList f |> MList.map (\(a, b) -> (a, V.toComparable b))



-- BUILD


build : Stuff.PackageCache -> MVar (Map.Map Pkg.Comparable (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set FingerprintComparable -> IO d e f g h Dep
build cache depsMVar pkg (Solver.Details vsn _) f fs =
  IO.bind (Outline.read (Stuff.package cache pkg vsn)) <| \eitherOutline ->
  case eitherOutline of
    Left _ ->
      IO.return <| Left <| Just <| Exit.BD_BadBuild pkg vsn f

    Right (Outline.App _) ->
      IO.return <| Left <| Just <| Exit.BD_BadBuild pkg vsn f

    Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
      IO.bind (MVar.read lensMVDepMap depsMVar) <| \allDeps ->
      IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVDep) (Map.intersection allDeps deps)) <| \directDeps ->
      case Map.sequenceA Either.pure Either.liftA2 directDeps of
        Left _ ->
          IO.return <| Left <| Nothing

        Right directArtifacts ->
          let src = Dir.addName (Stuff.package cache pkg vsn) "src" in
          let foreignDeps = gatherForeignInterfaces directArtifacts in
          let exposedDict = Map.fromKeys (\_ -> ()) (Outline.flattenExposed exposed) in
          IO.bind (getDocsStatus cache pkg vsn) <| \docsStatus ->
          IO.bind (MVar.newEmpty lensMVStatusMap) <| \mvar ->
          IO.bind (Map.traverseWithKey IO.pure IO.liftA2 (\k _ -> fork lensMVStatus (crawlModule foreignDeps mvar pkg src k)) exposedDict) <| \mvars ->
          IO.bind (MVar.write lensMVStatusMap mvar mvars) <| \_ ->
          IO.bind (Map.mapM_ IO.return IO.bind (MVar.read lensMVStatus) mvars) <| \_ ->
          IO.bind (IO.andThen (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVStatus)) <| MVar.read lensMVStatusMap mvar) <| \maybeStatuses ->
          case Map.sequenceA Just Maybe.map2 maybeStatuses of
            Nothing ->
              IO.return <| Left <| Just <| Exit.BD_BadBuild pkg vsn f

            Just statuses ->
              IO.bind (MVar.newEmpty lensMVResultMap) <| \rmvar ->
              IO.bind (Map.traverse IO.pure IO.liftA2 (\v -> fork lensMVResult (compile pkg rmvar v)) statuses) <| \rmvars ->
              IO.bind (MVar.write lensMVResultMap rmvar rmvars) <| \_ ->
              IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVResult) rmvars) <| \maybeResults ->
              case Map.sequenceA Just Maybe.map2 maybeResults of
                Nothing ->
                  IO.return <| Left <| Just <| Exit.BD_BadBuild pkg vsn f

                Just results ->
                  let
                    path = Dir.addName (Stuff.package cache pkg vsn) "artifacts.dat"
                    ifaces = gatherInterfaces exposedDict results
                    objects = gatherObjects results
                    artifacts = Artifacts ifaces objects
                    fingerprints = Set.insert (toComparable f) fs
                  in
                  IO.bind (writeDocs cache pkg vsn docsStatus results) <| \_ ->
                  IO.bind (File.writeBinary bArtifactCache path (ArtifactCache fingerprints artifacts)) <| \_ ->
                  IO.return (Right artifacts)



-- GATHER


gatherObjects : Map.Map ModuleName.Raw TResult -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results


addLocalGraph : ModuleName.Raw -> TResult -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs   -> Opt.addLocalGraph objs graph
    RForeign _      -> graph
    RKernelLocal cs -> Opt.addKernel (Name.getKernel name) cs graph
    RKernelForeign  -> graph


gatherInterfaces : Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw TResult -> Map.Map ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
  let
    onLeft  = Map.mapMissing identity (\_ () -> Debug.todo "compiler bug manifesting in Elm.Details.gatherInterfaces")
    onRight = Map.mapMaybeMissing identity     (\_    iface -> toLocalInterface I.private iface)
    onBoth  = Map.zipWithMaybeMatched identity (\_ () iface -> toLocalInterface I.public  iface)
  in
  Map.mergeA identity identity onLeft onRight onBoth exposed artifacts


toLocalInterface : (I.Interface -> a) -> TResult -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ -> Just (func iface)
    RForeign _     -> Nothing
    RKernelLocal _ -> Nothing
    RKernelForeign -> Nothing



-- GATHER FOREIGN INTERFACES


type ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface


gatherForeignInterfaces : Map.Map Pkg.Comparable Artifacts -> Map.Map ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
  let
    finalize : I.Interface -> TList I.Interface -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _::_ -> ForeignAmbiguous

    gather : Pkg.Comparable -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic : I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface  -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing
  in
  Map.map (OneOrMore.destruct finalize) <|
    Map.foldrWithKey gather Map.empty directArtifacts



-- CRAWL


type alias StatusDict =
  Map.Map ModuleName.Raw (MVar (Maybe Status))


type Status
  = SLocal (Map.Map ModuleName.Raw ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal (TList Kernel.Chunk)
  | SKernelForeign


crawlModule : Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> () -> IO d e f g h (Maybe Status)
crawlModule foreignDeps mvar pkg src name () =
  let path = Dir.addExtension (Dir.addNames src (ModuleName.toFileNames name)) "elm" in
  IO.bind (File.exists path) <| \exists ->
  case Map.lookup name foreignDeps of
    Just ForeignAmbiguous ->
      IO.return Nothing

    Just (ForeignSpecific iface) ->
      if exists
      then IO.return Nothing
      else IO.return (Just (SForeign iface))

    Nothing ->
      if exists then
        crawlFile foreignDeps mvar pkg src name path

      else if Pkg.isKernel pkg && Name.isKernel name then
        crawlKernel foreignDeps mvar pkg src name

      else
        IO.return Nothing


crawlFile : Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> FilePath -> IO d e f g h (Maybe Status)
crawlFile foreignDeps mvar pkg src expectedName path =
  IO.bind (File.readUtf8 path) <| \bytes ->
  case Parse.fromByteString (Parse.Package pkg) bytes of
    Right ((Src.Module (Just (A.At _ actualName)) _ imports _ _ _ _ _) as modul) ->
      if expectedName == actualName then
        IO.bind (crawlImports foreignDeps mvar pkg src imports) <| \deps ->
        IO.return (Just (SLocal deps modul))
      else
        IO.return Nothing

    _ ->
      IO.return Nothing


crawlImports : Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> TList Src.Import -> IO d e f g h (Map.Map ModuleName.Raw ())
crawlImports foreignDeps mvar pkg src imports =
  IO.bind (MVar.read lensMVStatusMap mvar) <| \statusDict ->
  let deps = Map.fromList (MList.map (\i -> (Src.getImportName i, ())) imports) in
  let news = Map.difference deps statusDict in
  IO.bind (Map.traverseWithKey IO.pure IO.liftA2 (\k _ -> fork lensMVStatus (crawlModule foreignDeps mvar pkg src k)) news) <| \mvars ->
  IO.bind (MVar.write lensMVStatusMap mvar (Map.union mvars statusDict)) <| \_ ->
  IO.bind (Map.mapM_ IO.return IO.bind (MVar.read lensMVStatus) mvars) <| \_ ->
  IO.return deps


crawlKernel : Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> IO d e f g h (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
  let path = Dir.addExtension (Dir.addNames src (ModuleName.toFileNames name)) "js" in
  IO.bind (File.exists path) <| \exists ->
  if exists
    then
      IO.bind (File.readUtf8 path) <| \bytes ->
      case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
        Nothing ->
          IO.return Nothing

        Just (Kernel.Content imports chunks) ->
          IO.bind (crawlImports foreignDeps mvar pkg src imports) <| \_ ->
          IO.return (Just (SKernelLocal chunks))
    else
      IO.return (Just SKernelForeign)


getDepHome : ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous                          -> Nothing



-- COMPILE


type TResult
  = RLocal I.Interface Opt.LocalGraph
  | RForeign I.Interface
  | RKernelLocal (TList Kernel.Chunk)
  | RKernelForeign


compile : Pkg.Name -> MVar (Map.Map ModuleName.Raw (MVar (Maybe TResult))) -> Status -> () -> IO d e f g h (Maybe TResult)
compile pkg mvar status () =
  case status of
    SLocal deps modul ->
      IO.bind (MVar.read lensMVResultMap mvar) <| \resultsDict ->
      IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVResult) (Map.intersection resultsDict deps)) <| \maybeResults ->
      case Map.sequenceA Just Maybe.map2 maybeResults of
        Nothing ->
          IO.return Nothing

        Just results ->
          case Compile.compile pkg (Map.mapMaybe getInterface results) modul of
            Left _ ->
              IO.return Nothing

            Right (Compile.Artifacts canonical annotations objects) ->
              let
                ifaces = I.fromModule pkg canonical annotations
              in
              IO.return (Just (RLocal ifaces objects))

    SForeign iface ->
      IO.return (Just (RForeign iface))

    SKernelLocal chunks ->
      IO.return (Just (RKernelLocal chunks))

    SKernelForeign ->
      IO.return (Just RKernelForeign)


getInterface : TResult -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ -> Just iface
    RForeign iface -> Just iface
    RKernelLocal _ -> Nothing
    RKernelForeign -> Nothing



-- MAKE DOCS


type DocsStatus
  = DocsNeeded
  | DocsNotNeeded


getDocsStatus : Stuff.PackageCache -> Pkg.Name -> V.Version -> IO d e f g h DocsStatus
getDocsStatus cache pkg vsn =
  IO.bind (File.exists (Dir.addName (Stuff.package cache pkg vsn) "docs.json")) <| \exists ->
  if exists
    then IO.return DocsNotNeeded
    else IO.return DocsNeeded


writeDocs : Stuff.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Map.Map ModuleName.Raw TResult -> IO d e f g h ()
writeDocs _ _ _ status _ =
  case status of
    DocsNeeded ->
      -- TODO: Builder.Elm.Details.writeDocs
      --E.writeUgly (Dir.joinPath [Stuff.package cache pkg vsn, "docs.json"]) <|
      --  Docs.encode <| Map.mapMaybe toDocs results
      IO.return ()

    DocsNotNeeded ->
      IO.return ()



-- DOWNLOAD PACKAGE


downloadPackage : Stuff.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> IO d e f g h (Either Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
  let
    url = Website.metadata pkg vsn "endpoint.json"
  in
  IO.bind (Http.get manager url [] identity (IO.return << Right)) <| \eitherByteString ->

  case eitherByteString of
    Left err ->
      IO.return <| Left <| Exit.PP_BadEndpointRequest err

    Right byteString ->
      case D.fromByteString endpointDecoder byteString of
        Left _ ->
          IO.return <| Left <| Exit.PP_BadEndpointContent url

        Right (endpoint, _) ->
          Http.getArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) <|
            \archive ->
              {- if expectedHash == Http.shaToChars sha
              then -} IO.fmap Right <| File.writePackage (Stuff.package cache pkg vsn) archive
              --else IO.return <| Left <| Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)


endpointDecoder : D.Decoder e (String, String)
endpointDecoder =
  D.bind (D.field "url" D.string) <| \url ->
  D.bind (D.field "hash" D.string) <| \hash ->
  D.return (url, hash)



-- BINARY


bDetails : B.Binary Details
bDetails =
  B.bin5
    (\a b c d e -> Details a b c d e ArtifactsCached)
    (\(Details a b c d e _) -> B.T5 a b c d e)
      File.bTime
      bValidOutline
      B.bWord64
      (B.bMap ModuleName.bRaw bLocal)
      (B.bMap ModuleName.bRaw bForeign)


bValidOutline : B.Binary ValidOutline
bValidOutline =
  B.custom "binary encoding of ValidOutline was corrupted"
    (\p0 p1 validOutline ->
      case validOutline of
        ValidApp a     -> p0 a
        ValidPkg a b c -> p1 a b c
    )
    |> B.var1 0 ValidApp (NE.bTList Outline.bSrcDir)
    |> B.var3 1 ValidPkg Pkg.bName (B.bTList ModuleName.bRaw) (B.bMap Pkg.bComparable V.bVersion)
    |> B.finish


bLocal : B.Binary Local
bLocal =
  B.bin6 Local (\(Local a b c d e f) -> B.T6 a b c d e f)
    B.bPath
    File.bTime
    (B.bTList ModuleName.bRaw)
    B.bBool
    B.bWord64
    B.bWord64


bForeign : B.Binary Foreign
bForeign =
  B.bin2 Foreign (\(Foreign a b) -> B.T2 a b) Pkg.bName (B.bTList Pkg.bName)


bInterfaces : B.Binary Interfaces
bInterfaces =
  B.bMap ModuleName.bComparable I.bDependencyInterface


bArtifacts : B.Binary Artifacts
bArtifacts =
  B.bin2 Artifacts (\(Artifacts a b) -> B.T2 a b)
    (B.bMap ModuleName.bRaw I.bDependencyInterface)
    Opt.bGlobalGraph


bArtifactCache : B.Binary ArtifactCache
bArtifactCache =
  B.bin2 ArtifactCache (\(ArtifactCache a b) -> B.T2 a b)
    (B.bSet bFingerprintComparable)
    bArtifacts


bFingerprintComparable : B.Binary FingerprintComparable
bFingerprintComparable =
  B.bTList (B.bTuple Pkg.bComparable V.bComparable)
