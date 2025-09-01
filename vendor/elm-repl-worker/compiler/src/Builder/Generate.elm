{- MANUALLY FORMATTED -}
module Builder.Generate exposing
  ( debug
  , dev
  , prod
  , repl
  --
  , GlobalState
  , LocalState
  , initialState
  --
  , async
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.File as File
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.Name as N
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Generate.JavaScript as JS
import Compiler.Generate.Mode as Mode
import Compiler.Nitpick.Debug as Nitpick
import Extra.System.Dir exposing (FilePath)
import Extra.System.MVar exposing (MVar)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Extra.System.IO as IO
import Extra.System.MVar as MVar
import Global


-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?



-- PUBLIC STATE


type alias GlobalState f g h =
  Build.GlobalState (LocalState f g h) f g h

type LocalState f g h = LocalState
  {- mvLocalGraph -} (MVar.State (GlobalState f g h) (Maybe Opt.LocalGraph))
  {- mvTypes -} (MVar.State (GlobalState f g h) (Maybe Extract.Types))


initialState : LocalState f g h
initialState = LocalState
  {- mvLocalGraph -} (MVar.initialState "LocalGraph")
  {- mvTypes -} (MVar.initialState "Types")


lensMVLocalGraph : Lens (GlobalState f g h) (MVar.State (GlobalState f g h) (Maybe Opt.LocalGraph))
lensMVLocalGraph =
  { getter = \(Global.State _ _ _ _ (LocalState x _ ) _ _ _) -> x
  , setter = \x (Global.State a b c d (LocalState _ bi) f g h) -> Global.State a b c d (LocalState x bi) f g h
  }

lensMVTypes : Lens (GlobalState f g h) (MVar.State (GlobalState f g h) (Maybe Extract.Types))
lensMVTypes =
  { getter = \(Global.State _ _ _ _ (LocalState _ x) _ _ _) -> x
  , setter = \x (Global.State a b c d (LocalState ai _) f g h) -> Global.State a b c d (LocalState ai x) f g h
  }



-- PRIVATE IO


type alias IO f g h v =
  IO.IO (GlobalState f g h) v



-- GENERATORS


type alias Task z f g h v =
  Task.Task z (GlobalState f g h) Exit.Generate v


debug : FilePath -> Details.Details -> Build.Artifacts -> Task z f g h String
debug root details (Build.Artifacts pkg ifaces roots modules) =
  Task.bind (loadObjects root details modules) <| \loading ->
  Task.bind (loadTypes root ifaces modules) <| \types ->
  Task.bind (finalizeObjects loading) <| \objects ->
  let mode = Mode.Dev (Mode.DevDebug types) in
  let graph = objectsToGlobalGraph objects in
  let mains = gatherMains pkg objects roots in
  Task.return <| JS.generate mode graph mains


dev : FilePath -> Details.Details -> Build.Artifacts -> Task z f g h String
dev root details (Build.Artifacts pkg _ roots modules) =
  Task.bind (Task.andThen finalizeObjects <| loadObjects root details modules) <| \objects ->
  let mode = Mode.Dev Mode.DevNormal in
  let graph = objectsToGlobalGraph objects in
  let mains = gatherMains pkg objects roots in
  Task.return <| JS.generate mode graph mains


{- NEW: async -}
async : FilePath -> Details.Details -> Build.Artifacts -> Task z f g h String
async root details (Build.Artifacts pkg _ roots modules) =
  Task.bind (Task.andThen finalizeObjects <| loadObjects root details modules) <| \objects ->
  let mode = Mode.Dev (Mode.DevAsync True Set.empty) in
  let graph = objectsToGlobalGraph objects in
  let mains = gatherMains pkg objects roots in
  Task.return <| JS.generate mode graph mains


prod : FilePath -> Details.Details -> Build.Artifacts -> Task z f g h String
prod root details (Build.Artifacts pkg _ roots modules) =
  Task.bind (Task.andThen finalizeObjects <| loadObjects root details modules) <| \objects ->
  Task.bind (checkForDebugUses objects) <| \_ ->
  let graph = objectsToGlobalGraph objects in
  let mode = Mode.Prod (Mode.shortenFieldNames graph) in
  let mains = gatherMains pkg objects roots in
  Task.return <| JS.generate mode graph mains


repl : FilePath -> Details.Details -> Bool -> Bool -> Build.ReplArtifacts -> N.Name -> Task z f g h (JS.CodeKind, String)
repl root details ansi htmlEnabled (Build.ReplArtifacts home modules localizer annotations) name =
  Task.bind (Task.andThen finalizeObjects <| loadObjects root details modules) <| \objects ->
  let graph = objectsToGlobalGraph objects in
  Task.return <| JS.generateForRepl ansi htmlEnabled localizer graph home name (Map.ex annotations name)



-- CHECK FOR DEBUG


checkForDebugUses : Objects -> Task z f g h ()
checkForDebugUses (Objects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    []    -> Task.return ()
    m::ms -> Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains : Pkg.Name -> Objects -> NE.TList Build.Root -> Map.Map ModuleName.Comparable Opt.Main
gatherMains pkg (Objects _ locals) roots =
  Map.fromList <| MMaybe.mapMaybe (lookupMain pkg locals) (NE.toList roots)


lookupMain : Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Comparable, Opt.Main)
lookupMain pkg locals root =
  let
    toPair name (Opt.LocalGraph maybeMain _ _) =
      Maybe.map (Tuple.pair (ModuleName.toComparable <| ModuleName.Canonical pkg name)) <| maybeMain
  in
  case root of
    Build.Inside  name   -> Maybe.andThen (toPair name) <| Map.lookup name locals
    Build.Outside name g -> toPair name g



-- LOADING OBJECTS


type LoadingObjects =
  LoadingObjects
    {- foreign_mvar -} (MVar (Maybe Opt.GlobalGraph))
    {- local_mvars -} (Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph)))


loadObjects : FilePath -> Details.Details -> TList Build.Module -> Task z f g h LoadingObjects
loadObjects root details modules =
  Task.io <|
    IO.bind (Details.loadObjects root details) <| \mvar ->
    IO.bind (MList.traverse IO.pure IO.liftA2 (loadObject root) modules) <| \mvars ->
    IO.return <| LoadingObjects mvar (Map.fromList mvars)


loadObject : FilePath -> Build.Module -> IO f g h (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      IO.bind (MVar.new lensMVLocalGraph (Just graph)) <| \mvar ->
      IO.return (name, mvar)

    Build.Cached name _ _ ->
      IO.bind (MVar.newEmpty lensMVLocalGraph) <| \mvar ->
      IO.bind (MVar.wait lensMVLocalGraph mvar <| \() -> File.readBinary Opt.bLocalGraph (Stuff.elmo root name)) <| \_ ->
      IO.return (name, mvar)



-- FINALIZE OBJECTS


type Objects =
  Objects
    {- foreign -} Opt.GlobalGraph
    {- locals -} (Map.Map ModuleName.Raw Opt.LocalGraph)


finalizeObjects : LoadingObjects -> Task z f g h Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio identity <|
    IO.bind (MVar.read Details.lensMVGlobalGraph mvar) <| \result ->
    IO.bind (Map.traverse IO.pure IO.liftA2 (MVar.read lensMVLocalGraph) mvars) <| \results ->
    case Maybe.map2 Objects result (Map.sequenceA Just Maybe.map2 results) of
      Just loaded -> IO.return (Right loaded)
      Nothing     -> IO.return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph : Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  Map.foldr Opt.addLocalGraph globals locals



-- LOAD TYPES


loadTypes : FilePath -> Map.Map ModuleName.Comparable I.DependencyInterface -> TList Build.Module -> Task z f g h Extract.Types
loadTypes root ifaces modules =
  Task.eio identity <|
    IO.bind (MList.traverse IO.pure IO.liftA2 (loadTypesHelp root) modules) <| \mvars ->
    let foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces)) in
    IO.bind (MList.traverse IO.pure IO.liftA2 (MVar.read lensMVTypes) mvars) <| \results ->
    case MList.sequenceA Just Maybe.map2 results of
      Just ts -> IO.return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
      Nothing -> IO.return (Left Exit.GenerateCannotLoadArtifacts)


loadTypesHelp : FilePath -> Build.Module -> IO f g h (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
  case modul of
    Build.Fresh name iface _ ->
      MVar.new lensMVTypes (Just (Extract.fromInterface name iface))

    Build.Cached name _ ciMVar ->
      IO.bind (MVar.read Build.lensMVCachedInterface ciMVar) <| \cachedInterface ->
      case cachedInterface of
      Build.Unneeded ->
        IO.bind (MVar.newEmpty lensMVTypes) <| \mvar ->
        IO.bind (MVar.wait lensMVTypes mvar <| \() ->
          IO.rmap
            (File.readBinary I.bInterface (Stuff.elmi root name))
            (Maybe.map (Extract.fromInterface name))) <| \_ ->
        IO.return mvar

      Build.Loaded iface ->
        MVar.new lensMVTypes (Just (Extract.fromInterface name iface))

      Build.Corrupted ->
        MVar.new lensMVTypes Nothing
