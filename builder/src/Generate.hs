{-# LANGUAGE BangPatterns #-}
module Generate
  ( debug
  , dev
  , devWithExportFlag
  , prod
  , repl
  , debugWithTypeScript
  , devWithTypeScript
  , prodWithTypeScript
  )
  where


import Prelude hiding (cycle, print)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2, sequence)
import qualified Data.ByteString.Builder as B
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE

import qualified AST.Optimized as Opt
import qualified Build
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.JavaScript as JS
import qualified Generate.TypeScript as TS
import qualified Generate.Mode as Mode
import qualified Nitpick.Debug as Nitpick
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff


import Lamdera ((&))
import qualified Lamdera
import qualified Lamdera.AppConfig

-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?



-- GENERATORS


type Task a =
  Task.Task Exit.Generate a


debug :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
debug root details (Build.Artifacts pkg ifaces roots modules) =
  do  loading <- loadObjects root details modules
      types   <- loadTypes root ifaces modules
      objects <- finalizeObjects loading
      let mode = Mode.Dev (Just types)
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      let mains = gatherMains pkg objects roots
      return $ JS.generate mode graph mains


debugWithTypeScript :: FilePath -> Details.Details -> Build.Artifacts -> Bool -> Task (B.Builder, B.Builder)
debugWithTypeScript root details artifacts@(Build.Artifacts pkg ifaces roots modules) exportAllFunctions =
  do  loading <- loadObjects root details modules
      types   <- loadTypes root ifaces modules
      objects <- finalizeObjects loading
      let mode = Mode.Dev (Just types)
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      let mains = if exportAllFunctions 
                  then gatherAllExports pkg objects roots
                  else gatherMains pkg objects roots
      let jsBuilder = JS.generate mode graph mains
      interfaces <- collectInterfaces root artifacts
      let tsBuilder = TS.generate interfaces mains graph
      return (jsBuilder, tsBuilder)


dev :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
dev root details artifacts =
  devWithExportFlag root details artifacts False

devWithExportFlag :: FilePath -> Details.Details -> Build.Artifacts -> Bool -> Task B.Builder
devWithExportFlag root details (Build.Artifacts pkg _ roots modules) exportAllFunctions =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let mode = Mode.Dev Nothing
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      let mains = if exportAllFunctions 
                  then gatherAllExports pkg objects roots
                  else gatherMains pkg objects roots
      return $ JS.generate mode graph mains


prod :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
prod root details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      checkForDebugUses objects
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      longNamesEnabled <- Task.io $ Lamdera.useLongNames
      let mode = Mode.Prod (Mode.shortenFieldNames graph)
                   & Lamdera.alternativeImplementationWhen longNamesEnabled
                       (Mode.Prod (Mode.legibleFieldNames graph))
      let mains = gatherMains pkg objects roots
      return $ JS.generate mode graph mains


repl :: FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task B.Builder
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      return $ JS.generateForRepl ansi localizer graph home name (annotations ! name)


devWithTypeScript :: FilePath -> Details.Details -> Build.Artifacts -> Bool -> Task (B.Builder, B.Builder)
devWithTypeScript root details artifacts@(Build.Artifacts pkg ifaces roots modules) exportAllFunctions =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let mode = Mode.Dev Nothing
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      let mains = if exportAllFunctions 
                  then gatherAllExports pkg objects roots
                  else gatherMains pkg objects roots
      let jsBuilder = JS.generate mode graph mains
      interfaces <- collectInterfaces root artifacts
      let tsBuilder = TS.generate interfaces mains graph
      return (jsBuilder, tsBuilder)


prodWithTypeScript :: FilePath -> Details.Details -> Build.Artifacts -> Bool -> Task (B.Builder, B.Builder)
prodWithTypeScript root details artifacts@(Build.Artifacts pkg ifaces roots modules) exportAllFunctions =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      checkForDebugUses objects
      let graph_ = objectsToGlobalGraph objects
      graph <- Task.io $ Lamdera.AppConfig.injectConfig graph_
      longNamesEnabled <- Task.io $ Lamdera.useLongNames
      let mode = Mode.Prod (Mode.shortenFieldNames graph)
                   & Lamdera.alternativeImplementationWhen longNamesEnabled
                       (Mode.Prod (Mode.legibleFieldNames graph))
      let mains = if exportAllFunctions 
                  then gatherAllExports pkg objects roots
                  else gatherMains pkg objects roots
      let jsBuilder = JS.generate mode graph mains
      interfaces <- collectInterfaces root artifacts
      let tsBuilder = TS.generate interfaces mains graph
      return (jsBuilder, tsBuilder)



-- COLLECT INTERFACES


collectInterfaces :: FilePath -> Build.Artifacts -> Task (Map.Map ModuleName.Canonical I.Interface)
collectInterfaces root (Build.Artifacts pkg deps _ modules) = Task.io $ do
  let
    freshInterfaces = Map.fromList
      [ (ModuleName.Canonical pkg name, iface)
      | Build.Fresh name iface _ <- modules
      ]
  
  -- For cached modules, we need to load the interfaces from disk
  cachedInterfaces <- fmap Map.fromList $ fmap Maybe.catMaybes $ sequence
    [ do -- Try to load the interface from disk
         maybeIface <- File.readBinary (Stuff.elmi root name)
         case maybeIface of
           Just iface -> return $ Just (ModuleName.Canonical pkg name, iface)
           Nothing -> return Nothing
    | Build.Cached name _ _ <- modules
    ]
  
  let
    localInterfaces = Map.union freshInterfaces cachedInterfaces
    foreignInterfaces = Map.mapMaybe depInterfaceToInterface deps
  
  return $ Map.union localInterfaces foreignInterfaces


depInterfaceToInterface :: I.DependencyInterface -> Maybe I.Interface
depInterfaceToInterface dep =
  case dep of
    I.Public iface -> Just iface
    I.Private _ _ _ -> Nothing


-- CHECK FOR DEBUG


checkForDebugUses :: Objects -> Task ()
checkForDebugUses (Objects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    []   -> return ()
    m:ms -> Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains :: Pkg.Name -> Objects -> NE.List Build.Root -> Map.Map ModuleName.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
  Map.fromList $ Maybe.mapMaybe (lookupMain pkg locals) (NE.toList roots)

-- Gather all exports (including modules without main) for --export-all-functions
gatherAllExports :: Pkg.Name -> Objects -> NE.List Build.Root -> Map.Map ModuleName.Canonical Opt.Main
gatherAllExports pkg (Objects _ locals) roots =
  Map.fromList $ Maybe.mapMaybe (lookupMainOrExports pkg locals) (NE.toList roots)


lookupMain :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMain pkg locals root =
  let
    toPair name (Opt.LocalGraph maybeMain _ _) =
      (,) (ModuleName.Canonical pkg name) <$> maybeMain
  in
  case root of
    Build.Inside  name     -> toPair name =<< Map.lookup name locals
    Build.Outside name _ g -> toPair name g

-- For --export-all-functions, create a synthetic main that references all exports
lookupMainOrExports :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMainOrExports pkg locals root =
  case lookupMain pkg locals root of
    Just result -> Just result
    Nothing ->
      -- If no main, create a synthetic one that marks the module for export
      case root of
        Build.Inside name ->
          case Map.lookup name locals of
            Just (Opt.LocalGraph _ nodes _) ->
              -- Create a synthetic main that marks this module for export
              Just (ModuleName.Canonical pkg name, Opt.Static)
            Nothing -> Nothing
        Build.Outside name _ (Opt.LocalGraph _ nodes _) ->
          Just (ModuleName.Canonical pkg name, Opt.Static)



-- LOADING OBJECTS


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
    }


loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects root details modules =
  Task.io $
  do  mvar <- Details.loadObjects root details
      mvars <- traverse (loadObject root) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: FilePath -> Build.Module -> IO (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      do  mvar <- newMVar (Just graph)
          return (name, mvar)

    Build.Cached name _ _ ->
      do  mvar <- newEmptyMVar
          _ <- forkIO $ putMVar mvar =<< File.readBinary (Stuff.elmo root name)
          return (name, mvar)



-- FINALIZE OBJECTS


data Objects =
  Objects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- readMVar mvar
      results <- traverse readMVar mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals



-- LOAD TYPES


loadTypes :: FilePath -> Map.Map ModuleName.Canonical I.DependencyInterface -> [Build.Module] -> Task Extract.Types
loadTypes root ifaces modules =
  Task.eio id $
  do  mvars <- traverse (loadTypesHelp root) modules
      let !foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces))
      results <- traverse readMVar mvars
      case sequence results of
        Just ts -> return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
        Nothing -> return (Left Exit.GenerateCannotLoadArtifacts)


loadTypesHelp :: FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
  case modul of
    Build.Fresh name iface _ ->
      newMVar (Just (Extract.fromInterface name iface))

    Build.Cached name _ ciMVar ->
      do  cachedInterface <- readMVar ciMVar
          case cachedInterface of
            Build.Unneeded ->
              do  mvar <- newEmptyMVar
                  _ <- forkIO $
                    do  maybeIface <- File.readBinary (Stuff.elmi root name)
                        putMVar mvar (Extract.fromInterface name <$> maybeIface)
                  return mvar

            Build.Loaded iface ->
              newMVar (Just (Extract.fromInterface name iface))

            Build.Corrupted ->
              newMVar Nothing
