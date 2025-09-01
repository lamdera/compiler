{- MANUALLY FORMATTED -}
module Builder.Build exposing
  ( fromExposed
  , fromPaths
  , fromRepl
  , Artifacts(..)
  , Root(..)
  , Module(..)
  , CachedInterface(..)
  , ReplArtifacts(..)
  , DocsGoal, DocsGoalKind(..), ignoreDocs
  --
  , GlobalState
  , LocalState
  , initialState
  , lensMVCachedInterface
  --
  , findModulePath
  )


import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Source as Src
import Compiler.Compile as Compile
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Render.Type.Localizer as L
import Extra.Data.Graph as Graph
import Extra.System.Config as Config
import Extra.System.Dir as Dir exposing (FileName, FilePath)
import Extra.System.IO as IO
import Extra.Type.Either as Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.System.MVar as MVar exposing (MVar)
import Extra.Type.Set as Set
import Global
import Unicode as UChar



-- PUBLIC STATE


type alias GlobalState e f g h =
  Details.GlobalState (LocalState e f g h) e f g h


type LocalState e f g h = LocalState
  {- mvStatus -} (MVar.State (GlobalState e f g h) Status)
  {- mvStatusMap -} (MVar.State (GlobalState e f g h) (Map.Map ModuleName.Raw (MVar Status)))
  {- mvRootStatus -} (MVar.State (GlobalState e f g h) RootStatus)
  {- mvRootResult -} (MVar.State (GlobalState e f g h) RootResult)
  {- mvResult -} (MVar.State (GlobalState e f g h) Result)
  {- mvResultMap -} (MVar.State (GlobalState e f g h) ResultDict)
  {- mvCachedInterface -} (MVar.State (GlobalState e f g h) CachedInterface)


initialState : LocalState e f g h
initialState = LocalState
  {- mvStatus -} (MVar.initialState "Status")
  {- mvStatusMap -} (MVar.initialState "StatusMap")
  {- mvRootStatus -} (MVar.initialState "RootStatus")
  {- mvRootResult -} (MVar.initialState "RootResult")
  {- mvResult -} (MVar.initialState "Result")
  {- mvResultMap -} (MVar.initialState "ResultMap")
  {- mvCachedInterface -} (MVar.initialState "CachedInterface")


lensMVStatus : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) Status)
lensMVStatus =
  { getter = \(Global.State _ _ _ (LocalState x _ _ _ _ _ _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState _ bi ci di ei fi gi) e f g h) -> Global.State a b c (LocalState x bi ci di ei fi gi) e f g h
  }

lensMVStatusMap : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) (Map.Map ModuleName.Raw (MVar Status)))
lensMVStatusMap =
  { getter = \(Global.State _ _ _ (LocalState _ x _ _ _ _ _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi _ ci di ei fi gi) e f g h) -> Global.State a b c (LocalState bi x ci di ei fi gi) e f g h
  }

lensMVRootStatus : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) RootStatus)
lensMVRootStatus =
  { getter = \(Global.State _ _ _ (LocalState _ _ x _ _ _ _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi ci _ di ei fi gi) e f g h) -> Global.State a b c (LocalState bi ci x di ei fi gi) e f g h
  }

lensMVRootResult : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) RootResult)
lensMVRootResult =
  { getter = \(Global.State _ _ _ (LocalState _ _ _ x _ _ _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi ci di _ ei fi gi) e f g h) -> Global.State a b c (LocalState bi ci di x ei fi gi) e f g h
  }

lensMVResult : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) Result)
lensMVResult =
  { getter = \(Global.State _ _ _ (LocalState _ _ _ _ x _ _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi ci di ei _ fi gi) e f g h) -> Global.State a b c (LocalState bi ci di ei x fi gi) e f g h
  }

lensMVResultMap : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) ResultDict)
lensMVResultMap =
  { getter = \(Global.State _ _ _ (LocalState _ _ _ _ _ x _) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi ci di ei fi _ gi) e f g h) -> Global.State a b c (LocalState bi ci di ei fi x gi) e f g h
  }

lensMVCachedInterface : Lens (GlobalState e f g h) (MVar.State (GlobalState e f g h) CachedInterface)
lensMVCachedInterface =
  { getter = \(Global.State _ _ _ (LocalState _ _ _ _ _ _ x) _ _ _ _) -> x
  , setter = \x (Global.State a b c (LocalState bi ci di ei fi gi _) e f g h) -> Global.State a b c (LocalState bi ci di ei fi gi x) e f g h
  }



-- PRIVATE IO


type alias IO e f g h v =
  IO.IO (GlobalState e f g h) v



-- ENVIRONMENT


type Env =
  Env
    {- root -} FilePath
    {- project -} Parse.ProjectType
    {- srcDirs -} (TList AbsoluteSrcDir)
    {- buildID -} Details.BuildID
    {- locals -} (Map.Map ModuleName.Raw Details.Local)
    {- foreigns -} (Map.Map ModuleName.Raw Details.Foreign)


makeEnv : FilePath -> Details.Details -> IO e f g h Env
makeEnv root (Details.Details _ validOutline buildID locals foreigns _) =
  case validOutline of
    Details.ValidApp givenSrcDirs ->
      IO.bind (MList.traverse IO.pure IO.liftA2 (toAbsoluteSrcDir root) (NE.toList givenSrcDirs)) <| \srcDirs ->
      IO.bind Config.additionalSrcDirs <| \additionalSrcDirNames ->
      let makeAbsolute name = IO.fmap AbsoluteSrcDir (Dir.makeAbsolute (Dir.fromString name)) in
      IO.bind (MList.traverse IO.pure IO.liftA2 makeAbsolute additionalSrcDirNames) <| \additionalSrcDirs ->
      IO.return <| Env root Parse.Application (srcDirs ++ additionalSrcDirs) buildID locals foreigns

    Details.ValidPkg pkg _ _ ->
      IO.bind (toAbsoluteSrcDir root (Outline.RelativeSrcDir (Dir.fromString "src"))) <| \srcDir ->
      IO.return <| Env root (Parse.Package pkg) [srcDir] buildID locals foreigns



-- SOURCE DIRECTORY


type AbsoluteSrcDir =
  AbsoluteSrcDir FilePath


toAbsoluteSrcDir : FilePath -> Outline.SrcDir -> IO e f g h AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
  IO.fmap AbsoluteSrcDir <| IO.return
    (
      case srcDir of
        Outline.AbsoluteSrcDir dir -> dir
        Outline.RelativeSrcDir dir -> Dir.combine root dir
    )


addRelative : AbsoluteSrcDir -> TList FileName -> FilePath
addRelative (AbsoluteSrcDir srcDir) segments =
  Dir.addExtension (Dir.addNames srcDir segments) "elm"



-- FORK


-- PERF try using IORef semephore on file crawl phase?
-- described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead
--
fork : MVar.Lens (GlobalState e f g h) v -> (() -> IO e f g h v) -> IO e f g h (MVar v)
fork = MVar.newWaiting


forkWithKey : MVar.Lens (GlobalState e f g h) w -> (comparable -> v -> () -> IO e f g h w) -> Map.Map comparable v -> IO e f g h (Map.Map comparable (MVar w))
forkWithKey lens func dict =
  Map.traverseWithKey IO.pure IO.liftA2 (\k v -> fork lens (func k v)) dict



-- FROM EXPOSED


fromExposed : FilePath -> Details.Details -> DocsGoal e f g h docs -> NE.TList ModuleName.Raw -> IO e f g h (Either Exit.BuildProblem docs)
fromExposed root details docsGoal ((NE.CList e es) as exposed) =
  IO.bind (makeEnv root details) <| \env ->
  IO.bind (Details.loadInterfaces root details) <| \dmvar ->

  -- crawl
  IO.bind (MVar.newEmpty lensMVStatusMap) <| \mvar ->
  let docsNeed = toDocsNeed docsGoal in
  IO.bind (Map.fromKeysA IO.pure IO.liftA2 (\k -> fork lensMVStatus <| crawlModule env mvar docsNeed k) (e::es)) <| \roots ->
  IO.bind (MVar.write lensMVStatusMap mvar roots) <| \_ ->
  IO.bind (Map.mapM_ IO.return IO.bind (MVar.read lensMVStatus) roots) <| \_ ->
  IO.bind (IO.andThen (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVStatus)) <| MVar.read lensMVStatusMap mvar) <| \statuses ->

  -- compile
  IO.bind (checkMidpoint dmvar statuses) <| \midpoint ->
  case midpoint of
    Left problem ->
      IO.return (Left (Exit.BuildProjectProblem problem))

    Right foreigns ->
      IO.bind (MVar.newEmpty lensMVResultMap) <| \rmvar ->
      IO.bind (forkWithKey lensMVResult (checkModule env foreigns rmvar) statuses) <| \resultMVars ->
      IO.bind (MVar.write lensMVResultMap rmvar resultMVars) <| \_ ->
      IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVResult) resultMVars) <| \results ->
      IO.bind (writeDetails root details results) <| \_ ->
      finalizeExposed root docsGoal exposed results



-- FROM PATHS


type Artifacts =
  Artifacts
    {- name -} Pkg.Name
    {- deps -} Dependencies
    {- roots -} (NE.TList Root)
    {- modules -} (TList Module)


type Module
  = Fresh ModuleName.Raw I.Interface Opt.LocalGraph
  | Cached ModuleName.Raw Bool (MVar CachedInterface)


type alias Dependencies =
  Map.Map ModuleName.Comparable I.DependencyInterface


fromPaths : FilePath -> Details.Details -> NE.TList FilePath -> IO e f g h (Either Exit.BuildProblem Artifacts)
fromPaths root details paths =
  IO.bind (makeEnv root details) <| \env ->

  IO.bind (findRoots env paths) <| \elroots ->
  case elroots of
    Left problem ->
      IO.return (Left (Exit.BuildProjectProblem problem))

    Right lroots ->
      -- crawl
      IO.bind (Details.loadInterfaces root details) <| \dmvar ->
      IO.bind (MVar.new lensMVStatusMap Map.empty) <| \smvar ->
      IO.bind (NE.traverse IO.pure IO.liftA2 IO.liftA2 (\v -> fork lensMVRootStatus <| crawlRoot env smvar v) lroots) <| \srootMVars ->
      IO.bind (NE.traverse IO.pure IO.liftA2 IO.liftA2 (MVar.read lensMVRootStatus) srootMVars) <| \sroots ->
      IO.bind (IO.andThen (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVStatus)) <| MVar.read lensMVStatusMap smvar) <| \statuses ->

      IO.bind (checkMidpointAndRoots dmvar statuses sroots) <| \midpoint ->
      case midpoint of
        Left problem ->
          IO.return (Left (Exit.BuildProjectProblem problem))

        Right foreigns ->
          -- compile
          IO.bind (MVar.newEmpty lensMVResultMap) <| \rmvar ->
          IO.bind (forkWithKey lensMVResult (checkModule env foreigns rmvar) statuses) <| \resultsMVars ->
          IO.bind (MVar.write lensMVResultMap rmvar resultsMVars) <| \_ ->
          IO.bind (NE.traverse IO.pure IO.liftA2 IO.liftA2 (\v -> fork lensMVRootResult (checkRoot env resultsMVars v)) sroots) <| \rrootMVars ->
          IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVResult) resultsMVars) <| \results ->
          IO.bind (writeDetails root details results) <| \_ ->
          IO.fmap (toArtifacts env foreigns results) <| NE.traverse IO.pure IO.liftA2 IO.liftA2 (MVar.read lensMVRootResult) rrootMVars



-- CRAWL


type alias StatusDict =
  Map.Map ModuleName.Raw (MVar Status)


type Status
  = SCached Details.Local
  | SChanged Details.Local String Src.Module
  | SBadImport Import.Problem
  | SBadSyntax FilePath File.Time String Syntax.Error
  | SForeign Pkg.Name
  | SKernel


crawlDeps : Env -> MVar StatusDict -> TList ModuleName.Raw -> v -> IO e f g h v
crawlDeps env mvar deps blockedValue =
  let
    crawlNew name () = fork lensMVStatus (crawlModule env mvar (DocsNeed False) name)
  in
  IO.bind (MVar.read lensMVStatusMap mvar) <| \statusDict ->
  let depsDict = Map.fromKeys (\_ -> ()) deps in
  let newsDict = Map.difference depsDict statusDict in
  IO.bind (Map.traverseWithKey IO.pure IO.liftA2 crawlNew newsDict) <| \statuses ->
  IO.bind (MVar.write lensMVStatusMap mvar (Map.union statuses statusDict)) <| \_ ->
  IO.bind (Map.mapM_ IO.return IO.bind (MVar.read lensMVStatus) statuses) <| \_ ->
  IO.return blockedValue


crawlModule : Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> () -> IO e f g h Status
crawlModule ((Env root projectType srcDirs buildID locals foreigns) as env) mvar docsNeed name () =
  let fileNames = ModuleName.toFileNames name in

  IO.bind (MList.filterM IO.pure IO.liftA2 File.exists (MList.map (\d -> addRelative d fileNames) srcDirs)) <| \paths ->

  case paths of
    [path] ->
      case Map.lookup name foreigns of
        Just (Details.Foreign dep _) ->
          IO.return <| SBadImport <| Import.Ambiguous path dep

        Nothing ->
          IO.bind (File.getTime path) <| \newTime ->
          case Map.lookup name locals of
            Nothing ->
              crawlFile env mvar name path newTime buildID

            Just ((Details.Local oldPath oldTime deps _ lastChange _) as local) ->
              if oldTime /= newTime || needsDocs docsNeed || not (isEquivalent root path oldPath)
              then crawlFile env mvar name path newTime lastChange
              else crawlDeps env mvar deps (SCached local)

    p1::p2::ps ->
      IO.return <| SBadImport <| Import.AmbiguousLocal (Dir.makeRelative root p1) (Dir.makeRelative root p2) (MList.map (Dir.makeRelative root) ps)

    [] ->
      case Map.lookup name foreigns of
        Just (Details.Foreign dep deps) ->
          case deps of
            [] ->
              IO.return <| SForeign dep

            d::ds ->
              IO.return <| SBadImport <| Import.AmbiguousForeign dep d ds

        Nothing ->
          if Name.isKernel name && Parse.isKernel projectType then
            IO.bind (File.exists (Dir.addExtension (Dir.addNames (Dir.fromString "src") (ModuleName.toFileNames name)) "js")) <| \exists ->
            IO.return <| if exists then SKernel else
              SBadImport Import.NotFound
          else
            IO.return <| SBadImport Import.NotFound


{- NEW: isEquivalent -}
isEquivalent : FilePath -> FilePath -> FilePath -> Bool
isEquivalent root path oldPath =
  let
    startsWith prefix testPath =
      case ( prefix, testPath ) of
        ( [], _ ) -> True
        ( _, [] ) -> False
        ( prefixHead :: prefixTail, testHead :: testTail ) -> prefixHead == testHead && startsWith prefixTail testTail
  in
  startsWith (Dir.getNames (Dir.makeRelative root path)) (Dir.getNames oldPath)


crawlFile : Env -> MVar StatusDict -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> IO e f g h Status
crawlFile ((Env root projectType _ buildID _ _) as env) mvar expectedName path time lastChange =
  IO.bind (File.readUtf8 (Dir.combine root path)) <| \source ->

  case Parse.fromByteString projectType source of
    Left err ->
      IO.return <| SBadSyntax path time source err

    Right ((Src.Module maybeActualName _ imports values _ _ _ _) as modul) ->
      case maybeActualName of
        Nothing ->
          IO.return <| SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

        Just ((A.At _ actualName) as name) ->
          if expectedName == actualName then
            let
              deps = MList.map Src.getImportName imports
              local = Details.Local path time deps (MList.any isMain values) lastChange buildID
            in
            crawlDeps env mvar deps (SChanged local source modul)
          else
            IO.return <| SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)


isMain : A.Located Src.Value -> Bool
isMain (A.At _ (Src.Value (A.At _ name) _ _ _)) =
  name == Name.l_main



-- CHECK MODULE


type alias ResultDict =
  Map.Map ModuleName.Raw (MVar Result)


type Result
  = RNew Details.Local I.Interface Opt.LocalGraph
  | RSame Details.Local I.Interface Opt.LocalGraph
  | RCached Bool Details.BuildID (MVar CachedInterface)
  | RNotFound Import.Problem
  | RProblem Error.Module
  | RBlocked
  | RForeign I.Interface
  | RKernel


type CachedInterface
  = Unneeded
  | Loaded I.Interface
  | Corrupted


checkModule : Env -> Dependencies -> MVar ResultDict -> ModuleName.Raw -> Status -> () -> IO e f g h Result
checkModule ((Env root projectType _ _ _ _) as env) foreigns resultsMVar name status () =
  case status of
    SCached ((Details.Local path time deps hasMain lastChange lastCompile) as local) ->
      IO.bind (MVar.read lensMVResultMap resultsMVar) <| \results ->
      IO.bind (checkDeps root results deps lastCompile) <| \depsStatus ->
      case depsStatus of
        DepsChange ifaces ->
          IO.bind (File.readUtf8 path) <| \source ->
            case Parse.fromByteString projectType source of
              Right modul -> compile env local source ifaces modul
              Left err ->
                IO.return <| RProblem <|
                  Error.Module name path time source (Error.BadSyntax err)

        DepsSame _ _ ->
          IO.bind (MVar.new lensMVCachedInterface Unneeded) <| \mvar ->
          IO.return (RCached hasMain lastChange mvar)

        DepsBlock ->
          IO.return RBlocked

        DepsNotFound problems ->
          IO.bind (File.readUtf8 path) <| \source ->
          IO.return <| RProblem <| Error.Module name path time source <|
            case Parse.fromByteString projectType source of
              Right (Src.Module _ _ imports _ _ _ _ _) ->
                Error.BadImports (toImportErrors env results imports problems)

              Left err ->
                Error.BadSyntax err

    SChanged ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ imports _ _ _ _ _) as modul) ->
      IO.bind (MVar.read lensMVResultMap resultsMVar) <| \results ->
      IO.bind (checkDeps root results deps lastCompile) <| \depsStatus ->
      case depsStatus of
        DepsChange ifaces ->
          compile env local source ifaces modul

        DepsSame same cached ->
          IO.bind (loadInterfaces root same cached) <| \maybeLoaded ->
          case maybeLoaded of
            Nothing     -> IO.return RBlocked
            Just ifaces -> compile env local source ifaces modul

        DepsBlock ->
          IO.return RBlocked

        DepsNotFound problems ->
          IO.return <| RProblem <| Error.Module name path time source <|
            Error.BadImports (toImportErrors env results imports problems)

    SBadImport importProblem ->
      IO.return (RNotFound importProblem)

    SBadSyntax path time source err ->
      IO.return <| RProblem <| Error.Module name path time source <|
        Error.BadSyntax err

    SForeign home ->
      case Map.ex foreigns (ModuleName.toComparable <| ModuleName.Canonical home name) of
        I.Public iface -> IO.return (RForeign iface)
        I.Private _ _ _ -> Debug.todo <| "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ ModuleName.toChars name

    SKernel ->
      IO.return RKernel



-- CHECK DEPS


type DepsStatus
  = DepsChange (Map.Map ModuleName.Raw I.Interface)
  | DepsSame (TList Dep) (TList CDep)
  | DepsBlock
  | DepsNotFound (NE.TList (ModuleName.Raw, Import.Problem))


checkDeps : FilePath -> ResultDict -> TList ModuleName.Raw -> Details.BuildID -> IO e f g h DepsStatus
checkDeps root results deps lastCompile =
  checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type alias Dep = (ModuleName.Raw, I.Interface)
type alias CDep = (ModuleName.Raw, MVar CachedInterface)


checkDepsHelp : FilePath -> ResultDict -> TList ModuleName.Raw -> TList Dep -> TList Dep -> TList CDep -> TList (ModuleName.Raw,Import.Problem) -> Bool -> Details.BuildID -> Details.BuildID -> IO e f g h DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
  case deps of
    dep::otherDeps ->
      IO.bind (MVar.read lensMVResult (Map.ex results dep)) <| \result ->
      case result of
        RNew (Details.Local _ _ _ _ lastChange _) iface _ ->
          checkDepsHelp root results otherDeps ((dep,iface) :: new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

        RSame (Details.Local _ _ _ _ lastChange _) iface _ ->
          checkDepsHelp root results otherDeps new ((dep,iface) :: same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

        RCached _ lastChange mvar ->
          checkDepsHelp root results otherDeps new same ((dep,mvar) :: cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

        RNotFound prob ->
          checkDepsHelp root results otherDeps new same cached ((dep,prob) :: importProblems) True lastDepChange lastCompile

        RProblem _ ->
          checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

        RBlocked ->
          checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

        RForeign iface ->
          checkDepsHelp root results otherDeps new ((dep,iface) :: same) cached importProblems isBlocked lastDepChange lastCompile

        RKernel ->
          checkDepsHelp root results otherDeps new same cached importProblems isBlocked lastDepChange lastCompile


    [] ->
      case MList.reverse importProblems of
        p::ps ->
          IO.return <| DepsNotFound (NE.CList p ps)

        [] ->
          if isBlocked then
            IO.return <| DepsBlock

          else if MList.null new && lastDepChange <= lastCompile then
            IO.return <| DepsSame same cached

          else
            IO.bind (loadInterfaces root same cached) <| \maybeLoaded ->
            case maybeLoaded of
              Nothing     -> IO.return DepsBlock
              Just ifaces -> IO.return <| DepsChange <| Map.union (Map.fromList new) ifaces


-- TO IMPORT ERROR


toImportErrors : Env -> ResultDict -> TList Src.Import -> NE.TList (ModuleName.Raw, Import.Problem) -> NE.TList Import.Error
toImportErrors (Env _ _ _ _ locals foreigns) results imports problems =
  let
    knownModules =
      Set.unions
        [ Map.keysSet foreigns
        , Map.keysSet locals
        , Map.keysSet results
        ]

    unimportedModules =
      Set.difference knownModules (Set.fromList (MList.map Src.getImportName imports))

    regionDict =
      Map.fromList (MList.map (\(Src.Import (A.At region name) _ _) -> (name, region)) imports)

    toError (name, problem) =
      Import.Error (Map.ex regionDict name) name unimportedModules problem
  in
  NE.fmap toError problems



-- LOAD CACHED INTERFACES


loadInterfaces : FilePath -> TList Dep -> TList CDep -> IO e f g h (Maybe (Map.Map ModuleName.Raw I.Interface))
loadInterfaces root same cached =
  IO.bind (MList.traverse IO.pure IO.liftA2 (loadInterface root) cached) <| \maybeLoaded ->
  case MList.sequenceA Just Maybe.map2 maybeLoaded of
    Nothing ->
      IO.return Nothing

    Just loaded ->
      IO.return <| Just <| Map.union (Map.fromList loaded) (Map.fromList same)


loadInterface : FilePath -> CDep -> IO e f g h (Maybe Dep)
loadInterface root (name, ciMvar) =
  IO.bind (MVar.read lensMVCachedInterface ciMvar) <| \cachedInterface ->
  case cachedInterface of
    Corrupted ->
      IO.bind (MVar.write lensMVCachedInterface ciMvar cachedInterface) <| \_ ->
      IO.return Nothing

    Loaded iface ->
      IO.bind (MVar.write lensMVCachedInterface ciMvar cachedInterface) <| \_ ->
      IO.return (Just (name, iface))

    Unneeded ->
      IO.bind (File.readBinary I.bInterface (Stuff.elmi root name)) <| \maybeIface ->
      case maybeIface of
        Nothing ->
          IO.bind (MVar.write lensMVCachedInterface ciMvar Corrupted) <| \_ ->
          IO.return Nothing

        Just iface ->
          IO.bind (MVar.write lensMVCachedInterface ciMvar (Loaded iface)) <| \_ ->
          IO.return (Just (name, iface))



-- CHECK PROJECT


checkMidpoint : MVar (Maybe Dependencies) -> Map.Map ModuleName.Raw Status -> IO e f g h (Either Exit.BuildProjectProblem Dependencies)
checkMidpoint dmvar statuses =
  case checkForCycles statuses of
    Nothing ->
      IO.bind (MVar.read Details.lensMVInterfaces dmvar) <| \maybeForeigns ->
      case maybeForeigns of
        Nothing -> IO.return (Left Exit.BP_CannotLoadDependencies)
        Just fs -> IO.return (Right fs)

    Just (NE.CList name names) ->
      IO.bind (MVar.read Details.lensMVInterfaces dmvar) <| \_ ->
      IO.return (Left (Exit.BP_Cycle name names))


checkMidpointAndRoots : MVar (Maybe Dependencies) -> Map.Map ModuleName.Raw Status -> NE.TList RootStatus -> IO e f g h (Either Exit.BuildProjectProblem Dependencies)
checkMidpointAndRoots dmvar statuses sroots =
  case checkForCycles statuses of
    Nothing ->
      case checkUniqueRoots statuses sroots of
        Nothing ->
          IO.bind (MVar.read Details.lensMVInterfaces dmvar) <| \maybeForeigns ->
          case maybeForeigns of
            Nothing -> IO.return (Left Exit.BP_CannotLoadDependencies)
            Just fs -> IO.return (Right fs)

        Just problem ->
          IO.bind (MVar.read Details.lensMVInterfaces dmvar) <| \_ ->
          IO.return (Left problem)

    Just (NE.CList name names) ->
      IO.bind (MVar.read Details.lensMVInterfaces dmvar) <| \_ ->
      IO.return (Left (Exit.BP_Cycle name names))



-- CHECK FOR CYCLES


checkForCycles : Map.Map ModuleName.Raw Status -> Maybe (NE.TList ModuleName.Raw)
checkForCycles modules =
  let
    graph = Map.foldrWithKey addToGraph [] modules
    sccs = Graph.stronglyConnComp graph
  in
  checkForCyclesHelp sccs


checkForCyclesHelp : TList (Graph.SCC ModuleName.Raw) -> Maybe (NE.TList ModuleName.Raw)
checkForCyclesHelp sccs =
  case sccs of
    [] ->
      Nothing

    scc::otherSccs ->
      case scc of
        Graph.AcyclicSCC _     -> checkForCyclesHelp otherSccs
        Graph.CyclicSCC []     -> checkForCyclesHelp otherSccs
        Graph.CyclicSCC (m::ms) -> Just (NE.CList m ms)


type alias Node =
  ( ModuleName.Raw, ModuleName.Raw, TList ModuleName.Raw )


addToGraph : ModuleName.Raw -> Status -> TList Node -> TList Node
addToGraph name status graph =
  let
    dependencies =
      case status of
        SCached  (Details.Local _ _ deps _ _ _)     -> deps
        SChanged (Details.Local _ _ deps _ _ _) _ _ -> deps
        SBadImport _                                -> []
        SBadSyntax _ _ _ _                          -> []
        SForeign _                                  -> []
        SKernel                                     -> []
  in
  (name, name, dependencies) :: graph



-- CHECK UNIQUE ROOTS


checkUniqueRoots : Map.Map ModuleName.Raw Status -> NE.TList RootStatus -> Maybe Exit.BuildProjectProblem
checkUniqueRoots insides sroots =
  let
    outsidesDict =
      Map.fromListWith OneOrMore.more (MMaybe.mapMaybe rootStatusToNamePathPair (NE.toList sroots))
  in
  case Map.traverseWithKey Either.pure Either.liftA2 checkOutside outsidesDict of
    Left problem ->
      Just problem

    Right outsides ->
      case Map.sequence_ Right Either.bind (Map.intersectionWithKey checkInside outsides insides) of
        Right ()     -> Nothing
        Left problem -> Just problem


rootStatusToNamePathPair : RootStatus -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore FilePath)
rootStatusToNamePathPair sroot =
  case sroot of
    SInside _                                         -> Nothing
    SOutsideOk (Details.Local path _ _ _ _ _) _ modul -> Just (Src.getName modul, OneOrMore.one path)
    SOutsideErr _                                     -> Nothing


checkOutside : ModuleName.Raw -> OneOrMore.OneOrMore FilePath -> Either Exit.BuildProjectProblem FilePath
checkOutside name paths =
  case OneOrMore.destruct NE.CList paths of
    NE.CList p  []      -> Right p
    NE.CList p1 (p2::_) -> Left (Exit.BP_RootNameDuplicate name p1 p2)


checkInside : ModuleName.Raw -> FilePath -> Status -> Either Exit.BuildProjectProblem ()
checkInside name p1 status =
  case status of
    SCached  (Details.Local p2 _ _ _ _ _)     -> Left (Exit.BP_RootNameDuplicate name p1 p2)
    SChanged (Details.Local p2 _ _ _ _ _) _ _ -> Left (Exit.BP_RootNameDuplicate name p1 p2)
    SBadImport _                              -> Right ()
    SBadSyntax _ _ _ _                        -> Right ()
    SForeign _                                -> Right ()
    SKernel                                   -> Right ()



-- COMPILE MODULE


compile : Env -> Details.Local -> String -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO e f g h Result
compile (Env root projectType _ buildID _ _) (Details.Local path time deps main lastChange _) source ifaces modul =
  let
    pkg = projectTypeToPkg projectType
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      let name = Src.getName modul in
      let iface = I.fromModule pkg canonical annotations in
      let elmi = Stuff.elmi root name in
      IO.bind (File.writeBinary Opt.bLocalGraph (Stuff.elmo root name) objects) <| \_ ->
      IO.bind (File.readBinary I.bInterface elmi) <| \maybeOldi ->
      let
        otherwise () =
          -- iface may be lazy still
          IO.bind (File.writeBinary I.bInterface elmi iface) <| \_ ->
          let local = Details.Local path time deps main buildID buildID in
          IO.return (RNew local iface objects)
      in
      case maybeOldi of
        Just oldi ->
          -- TODO: Can we compare Interfaces?
          if oldi == iface then
            -- iface should be fully forced by equality check
            let local = Details.Local path time deps main lastChange buildID in
            IO.return (RSame local iface objects)
          else otherwise ()

        _ ->
          otherwise ()

    Left err ->
      IO.return <| RProblem <|
        Error.Module (Src.getName modul) path time source err


projectTypeToPkg : Parse.ProjectType -> Pkg.Name
projectTypeToPkg projectType =
  case projectType of
    Parse.Package pkg ->
      pkg

    Parse.Application ->
      Pkg.dummyName



-- WRITE DETAILS


writeDetails : FilePath -> Details.Details -> Map.Map ModuleName.Raw Result -> IO e f g h ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
  File.writeBinary Details.bDetails (Stuff.details root) <|
    Details.Details time outline buildID (Map.foldrWithKey addNewLocal locals results) foreigns extras


addNewLocal : ModuleName.Raw -> Result -> Map.Map ModuleName.Raw Details.Local -> Map.Map ModuleName.Raw Details.Local
addNewLocal name result locals =
  case result of
    RNew  local _ _ -> Map.insert name local locals
    RSame local _ _ -> Map.insert name local locals
    RCached _ _ _   -> locals
    RNotFound _     -> locals
    RProblem _      -> locals
    RBlocked        -> locals
    RForeign _      -> locals
    RKernel         -> locals



-- FINALIZE EXPOSED


finalizeExposed : FilePath -> DocsGoal e f g h docs -> NE.TList ModuleName.Raw -> Map.Map ModuleName.Raw Result -> IO e f g h (Either Exit.BuildProblem docs)
finalizeExposed root docsGoal exposed results =
  case MList.foldr (addImportProblems results) [] (NE.toList exposed) of
    p::ps ->
      IO.return <| Left <| Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.CList p ps))

    [] ->
      case Map.foldr addErrors [] results of
        []    -> IO.fmap Right <| finalizeDocs docsGoal results
        e::es -> IO.return <| Left <| Exit.BuildBadModules root e es


addErrors : Result -> TList Error.Module -> TList Error.Module
addErrors result errors =
  case result of
    RNew  _ _ _   ->    errors
    RSame _ _ _   ->    errors
    RCached _ _ _ ->    errors
    RNotFound _   ->    errors
    RProblem e    -> e::errors
    RBlocked      ->    errors
    RForeign _    ->    errors
    RKernel       ->    errors


addImportProblems : Map.Map ModuleName.Raw Result -> ModuleName.Raw -> TList (ModuleName.Raw, Import.Problem) -> TList (ModuleName.Raw, Import.Problem)
addImportProblems results name problems =
  case Map.ex results name of
    RNew  _ _ _   -> problems
    RSame _ _ _   -> problems
    RCached _ _ _ -> problems
    RNotFound p   -> (name, p) :: problems
    RProblem _    -> problems
    RBlocked      -> problems
    RForeign _    -> problems
    RKernel       -> problems



-- DOCS


type alias DocsGoal e f g h v =
  { kind : DocsGoalKind
  , finalize : Map.Map ModuleName.Raw Result -> IO e f g h v
  }

type DocsGoalKind
  = IgnoreDocs

ignoreDocs : DocsGoal e f g h ()
ignoreDocs =
  { kind = IgnoreDocs
  , finalize = finalizeIgnoreDocs
  }


type DocsNeed =
  DocsNeed Bool

needsDocs : DocsNeed -> Bool
needsDocs (DocsNeed b) = b


toDocsNeed : DocsGoal e f g h v -> DocsNeed
toDocsNeed goal =
  case goal.kind of
    IgnoreDocs -> DocsNeed False


finalizeDocs : DocsGoal e f g h docs -> Map.Map ModuleName.Raw Result -> IO e f g h docs
finalizeDocs goal results =
  goal.finalize results

finalizeIgnoreDocs : Map.Map ModuleName.Raw Result -> IO e f g h ()
finalizeIgnoreDocs _ =
  IO.return ()



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- FIND ROOT


type RootLocation
  = LInside ModuleName.Raw
  | LOutside FilePath


findRoots : Env -> NE.TList FilePath -> IO e f g h (Either Exit.BuildProjectProblem (NE.TList RootLocation))
findRoots env paths =
  IO.bind (NE.traverse IO.pure IO.liftA2 IO.liftA2 (getRootInfo env) paths) <| \einfos ->
  IO.return <| Either.andThen checkRoots <| NE.sequenceA Either.pure Either.liftA2 Either.liftA2 einfos


checkRoots : NE.TList RootInfo -> Either Exit.BuildProjectProblem (NE.TList RootLocation)
checkRoots infos =
  let
    toOneOrMore ((RootInfo absolute _ _) as loc) =
      (Dir.toString absolute, OneOrMore.one loc)

    fromOneOrMore loc locs =
      case locs of
        [] -> Right ()
        loc2::_ -> Left (Exit.BP_MainPathDuplicate (getRelative loc) (getRelative loc2))
  in
  Either.fmap (\_ -> NE.fmap getLocation infos) <|
    Map.traverse Either.pure Either.liftA2 (OneOrMore.destruct fromOneOrMore) <|
      Map.fromListWith OneOrMore.more <| MList.map toOneOrMore (NE.toList infos)



-- ROOT INFO


type RootInfo =
  RootInfo
    {- absolute -} FilePath
    {- relative -} FilePath
    {- location -} RootLocation

getRelative (RootInfo _ relative _) = relative
getLocation (RootInfo _ _ location) = location


getRootInfo : Env -> FilePath -> IO e f g h (Either Exit.BuildProjectProblem RootInfo)
getRootInfo env path =
  IO.bind (File.exists path) <| \exists ->
  if exists
    then IO.andThen (getRootInfoHelp env path) <| Dir.makeAbsolute path
    else IO.return (Left (Exit.BP_PathUnknown path))


getRootInfoHelp : Env -> FilePath -> FilePath -> IO e f g h (Either Exit.BuildProjectProblem RootInfo)
getRootInfoHelp (Env _ _ srcDirs _ _ _) path absolutePath =
  let
    (dirs, file) = Dir.splitLastName absolutePath
    (final, ext) = Dir.splitExtension file
  in
  if ext /= "elm"
  then
    IO.return <| Left <| Exit.BP_WithBadExtension path
  else
    let
      absoluteSegments = MList.reverse (final :: Dir.getNames dirs)
    in
    case MMaybe.mapMaybe (isInsideSrcDirByPath absoluteSegments) srcDirs of
      [] ->
        IO.return <| Right <| RootInfo absolutePath path (LOutside path)

      [(_, Right names)] ->
        let name = String.join "." names in
        IO.bind (MList.filterM IO.pure IO.liftA2 (isInsideSrcDirByName names) srcDirs) <| \matchingDirs ->
        case matchingDirs of
          d1::d2::_ ->
            let p1 = addRelative d1 names in
            let p2 = addRelative d2 names in
            IO.return <| Left <| Exit.BP_RootNameDuplicate name p1 p2

          _ ->
            IO.return <| Right <| RootInfo absolutePath path (LInside name)

      [(s, Left _)] ->
        IO.return <| Left <| Exit.BP_RootNameInvalid path s

      (s1,_)::(s2,_)::_ ->
        IO.return <| Left <| Exit.BP_WithAmbiguousSrcDir path s1 s2



isInsideSrcDirByName : TList FileName -> AbsoluteSrcDir -> IO e f g h Bool
isInsideSrcDirByName names srcDir =
  File.exists (addRelative srcDir names)


isInsideSrcDirByPath : TList FileName -> AbsoluteSrcDir -> Maybe (FilePath, Either (TList FileName) (TList FileName))
isInsideSrcDirByPath segments (AbsoluteSrcDir srcDir) =
  MMaybe.bind (dropPrefix (MList.reverse (Dir.getNames srcDir)) segments) <| \names ->
    if MList.all isGoodName names
    then Just (srcDir, Right names)
    else Just (srcDir, Left names)


isGoodName : String -> Bool
isGoodName name =
  case String.uncons name of
    Nothing ->
      False

    Just ( char, chars ) ->
      UChar.isUpper char && String.all (\c -> UChar.isAlphaNum c || c == '_') chars


-- INVARIANT: Dir.canonicalizePath has been run on both inputs
--
dropPrefix : TList FileName -> TList FileName -> Maybe (TList FileName)
dropPrefix roots paths =
  case roots of
    [] ->
      Just paths

    r::rs ->
      case paths of
        []   -> Nothing
        p::ps -> if r == p then dropPrefix rs ps else Nothing



-- CRAWL ROOTS


type RootStatus
  = SInside ModuleName.Raw
  | SOutsideOk Details.Local String Src.Module
  | SOutsideErr Error.Module


crawlRoot : Env -> MVar StatusDict -> RootLocation -> () -> IO e f g h RootStatus
crawlRoot ((Env _ projectType _ buildID _ _) as env) mvar root () =
  case root of
    LInside name ->
      IO.bind (MVar.newEmpty lensMVStatus) <| \statusMVar ->
      IO.bind (MVar.read lensMVStatusMap mvar) <| \statusDict ->
      IO.bind (MVar.write lensMVStatusMap mvar (Map.insert name statusMVar statusDict)) <| \_ ->
      IO.bind (IO.andThen (MVar.write lensMVStatus statusMVar) <| crawlModule env mvar (DocsNeed False) name ()) <| \_ ->
      IO.return (SInside name)

    LOutside path ->
      IO.bind (File.getTime path) <| \time ->
      IO.bind (File.readUtf8 path) <| \source ->
      case Parse.fromByteString projectType source of
        Right ((Src.Module _ _ imports values _ _ _ _) as modul) ->
          let deps = MList.map Src.getImportName imports in
          let local = Details.Local path time deps (MList.any isMain values) buildID buildID in
          crawlDeps env mvar deps (SOutsideOk local source modul)

        Left syntaxError ->
          IO.return <| SOutsideErr <|
            Error.Module "???" path time source (Error.BadSyntax syntaxError)



-- CHECK ROOTS


type RootResult
  = RInside ModuleName.Raw
  | ROutsideOk ModuleName.Raw I.Interface Opt.LocalGraph
  | ROutsideErr Error.Module
  | ROutsideBlocked




checkRoot : Env -> ResultDict -> RootStatus -> () -> IO e f g h RootResult
checkRoot ((Env root _ _ _ _ _) as env) results rootStatus () =
  case rootStatus of
    SInside name ->
      IO.return (RInside name)

    SOutsideErr err ->
      IO.return (ROutsideErr err)

    SOutsideOk ((Details.Local path time deps _ _ lastCompile) as local) source ((Src.Module _ _ imports _ _ _ _ _) as modul) ->
      IO.bind (checkDeps root results deps lastCompile) <| \depsStatus ->
      case depsStatus of
        DepsChange ifaces ->
          compileOutside env local source ifaces modul

        DepsSame same cached ->
          IO.bind (loadInterfaces root same cached) <| \maybeLoaded ->
          case maybeLoaded of
            Nothing     -> IO.return ROutsideBlocked
            Just ifaces -> compileOutside env local source ifaces modul

        DepsBlock ->
          IO.return ROutsideBlocked

        DepsNotFound problems ->
          IO.return <| ROutsideErr <| Error.Module (Src.getName modul) path time source <|
            Error.BadImports (toImportErrors env results imports problems)


compileOutside : Env -> Details.Local -> String -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO e f g h RootResult
compileOutside (Env _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = projectTypeToPkg projectType
    name = Src.getName modul
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      IO.return <| ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      IO.return <| ROutsideErr <| Error.Module name path time source errors



-- TO ARTIFACTS


type Root
  = Inside ModuleName.Raw
  | Outside ModuleName.Raw Opt.LocalGraph


toArtifacts : Env -> Dependencies -> Map.Map ModuleName.Raw Result -> NE.TList RootResult -> Either Exit.BuildProblem Artifacts
toArtifacts (Env root projectType _ _ _ _) foreigns results rootResults =
  case gatherProblemsOrMains results rootResults of
    Left (NE.CList e es) ->
      Left (Exit.BuildBadModules root e es)

    Right roots ->
      Right <| Artifacts (projectTypeToPkg projectType) foreigns roots <|
        Map.foldrWithKey addInside (NE.foldr addOutside [] rootResults) results


gatherProblemsOrMains : Map.Map ModuleName.Raw Result -> NE.TList RootResult -> Either (NE.TList Error.Module) (NE.TList Root)
gatherProblemsOrMains results (NE.CList rootResult rootResults) =
  let
    addResult result (es, roots) =
      case result of
        RInside n        -> (   es, Inside n    :: roots)
        ROutsideOk n _ o -> (   es, Outside n o :: roots)
        ROutsideErr e    -> (e::es,                roots)
        ROutsideBlocked  -> (   es,                roots)

    errors = Map.foldr addErrors [] results
  in
  case (rootResult, MList.foldr addResult (errors, []) rootResults) of
    (RInside n       , (   [], ms)) -> Right (NE.CList (Inside n) ms)
    (RInside _       , (e::es, _ )) -> Left  (NE.CList e es)
    (ROutsideOk n _ o, (   [], ms)) -> Right (NE.CList (Outside n o) ms)
    (ROutsideOk _ _ _, (e::es, _ )) -> Left  (NE.CList e es)
    (ROutsideErr e   , (   es, _ )) -> Left  (NE.CList e es)
    (ROutsideBlocked , (   [], _ )) -> Debug.todo "seems like elm-stuff/ is corrupted"
    (ROutsideBlocked , (e::es, _ )) -> Left  (NE.CList e es)


addInside : ModuleName.Raw -> Result -> TList Module -> TList Module
addInside name result modules =
  case result of
    RNew  _ iface objs  -> Fresh name iface objs :: modules
    RSame _ iface objs  -> Fresh name iface objs :: modules
    RCached main _ mvar -> Cached name main mvar :: modules
    RNotFound _         -> Debug.todo (badInside name)
    RProblem _          -> Debug.todo (badInside name)
    RBlocked            -> Debug.todo (badInside name)
    RForeign _          -> modules
    RKernel             -> modules


badInside : ModuleName.Raw -> String
badInside name =
  "Error from `" ++ name ++ "` should have been reported already."


addOutside : RootResult -> TList Module -> TList Module
addOutside root modules =
  case root of
    RInside _                  -> modules
    ROutsideOk name iface objs -> Fresh name iface objs :: modules
    ROutsideErr _              -> modules
    ROutsideBlocked            -> modules



--------------------------------------------------------------------------------
------ NOW FOR SOME REPL STUFF -------------------------------------------------
--------------------------------------------------------------------------------


-- FROM REPL


type ReplArtifacts =
  ReplArtifacts
    {- repl_home -} ModuleName.Canonical
    {- repl_modules -} (TList Module)
    {- repl_localizer -} L.Localizer
    {- repl_annotations -} (Map.Map Name.Name Can.Annotation)


fromRepl : FilePath -> Details.Details -> String -> IO e f g h (Either Exit.Repl ReplArtifacts)
fromRepl root details source =
  IO.bind (makeEnv root details) <| \((Env _ projectType _ _ _ _) as env) ->
  case Parse.fromByteString projectType source of
    Left syntaxError ->
      IO.return <| Left <| Exit.ReplBadInput source <| Error.BadSyntax syntaxError

    Right ((Src.Module _ _ imports _ _ _ _ _) as modul) ->
      IO.bind (Details.loadInterfaces root details) <| \dmvar ->

      let deps = MList.map Src.getImportName imports in
      IO.bind (MVar.new lensMVStatusMap Map.empty) <| \mvar ->
      IO.bind (crawlDeps env mvar deps ()) <| \_ ->

      IO.bind (IO.andThen (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVStatus)) <| MVar.read lensMVStatusMap mvar) <| \statuses ->
      IO.bind (checkMidpoint dmvar statuses) <| \midpoint ->

      case midpoint of
        Left problem ->
          IO.return <| Left <| Exit.ReplProjectProblem problem

        Right foreigns ->
          IO.bind (MVar.newEmpty lensMVResultMap) <| \rmvar ->
          IO.bind (forkWithKey lensMVResult (checkModule env foreigns rmvar) statuses) <| \resultMVars ->
          IO.bind (MVar.write lensMVResultMap rmvar resultMVars) <| \_ ->
          IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVResult) resultMVars) <| \results ->
          IO.bind (writeDetails root details results) <| \_ ->
          IO.bind (checkDeps root resultMVars deps 0) <| \depsStatus ->
          finalizeReplArtifacts env source modul depsStatus resultMVars results


finalizeReplArtifacts : Env -> String -> Src.Module -> DepsStatus -> ResultDict -> Map.Map ModuleName.Raw Result -> IO e f g h (Either Exit.Repl ReplArtifacts)
finalizeReplArtifacts ((Env root projectType _ _ _ _) as env) source ((Src.Module _ _ imports _ _ _ _ _) as modul) depsStatus resultMVars results =
  let
    pkg =
      projectTypeToPkg projectType

    compileInput ifaces =
      case Compile.compile pkg ifaces modul of
        Right (Compile.Artifacts canonical annotations objects) ->
          let
            h = Can.getName canonical
            m = Fresh (Src.getName modul) (I.fromModule pkg canonical annotations) objects
            ms = Map.foldrWithKey addInside [] results
          in
          IO.return <| Right <| ReplArtifacts h (m::ms) (L.fromModule modul) annotations

        Left errors ->
          IO.return <| Left <| Exit.ReplBadInput source errors
  in
  case depsStatus of
    DepsChange ifaces ->
      compileInput ifaces

    DepsSame same cached ->
      IO.bind (loadInterfaces root same cached) <| \maybeLoaded ->
      case maybeLoaded of
        Just ifaces -> compileInput ifaces
        Nothing     -> IO.return <| Left <| Exit.ReplBadCache

    DepsBlock ->
      case Map.foldr addErrors [] results of
        []    -> IO.return <| Left <| Exit.ReplBlocked
        e::es -> IO.return <| Left <| Exit.ReplBadLocalDeps root e es

    DepsNotFound problems ->
      IO.return <| Left <| Exit.ReplBadInput source <| Error.BadImports <|
        toImportErrors env resultMVars imports problems


{- NEW: findModulePath -}
findModulePath : FilePath -> Details.Details -> ModuleName.Raw -> IO e f g h (Maybe FilePath)
findModulePath root details name =
  IO.bind (makeEnv root details) <| \(Env _ _ srcDirs _ _ _) ->
  let fileNames = ModuleName.toFileNames name in
  IO.rmap (MList.filterM IO.pure IO.liftA2 File.exists (MList.map (\d -> addRelative d fileNames) srcDirs)) <| \paths ->
  case paths of
    [path] -> Just path
    _ -> Nothing
