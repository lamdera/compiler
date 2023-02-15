{-# OPTIONS_GHC -Wall #-}

module Ext.Query.Interfaces where

{- Helpers for loading cached interfaces after/outside compilation process -}

import Control.Monad (liftM2)
import qualified Data.Map as Map
import qualified Data.OneOrMore as OneOrMore
import Control.Concurrent.MVar

import qualified Data.NonEmptyList as NE
import qualified BackgroundWriter as BW
import qualified AST.Optimized as Opt
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Build
import qualified File
import qualified Stuff
import qualified Json.Encode as Encode

import Ext.Common


all :: [FilePath] -> IO (Map.Map ModuleName.Raw I.Interface)
all paths = do
  debug $ "Loading Interfaces.all on paths: " ++ show paths
  artifactsDeps <- allDepArtifacts
  ifacesProject <-
    case paths of
      [] -> error "Ext.Query.Interfaces.all must have at least one path specified!"
      path:paths -> allProjectInterfaces (NE.List path paths)

  pure $ Map.union ifacesProject (_ifaces artifactsDeps)


-- Takes Build.Artifacts and extracts project interfaces, loads all package dep interfaces, and merges them
artifactsToFullInterfaces :: Details.Details -> Build.Artifacts -> IO (Map.Map ModuleName.Raw I.Interface)
artifactsToFullInterfaces details artifacts = do

  ifaces <- extractInterfaces $ Build._modules artifacts

  artifactsDeps <- allDepArtifacts_ details
  pure $ Map.union ifaces (_ifaces artifactsDeps)



allGraph :: IO Opt.GlobalGraph
allGraph = do
  artifactsDeps <- allDepArtifacts
  pure $ _graph artifactsDeps


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.Interface
    , _graph :: Opt.GlobalGraph
    }


{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allDepArtifacts :: IO Artifacts
allDepArtifacts =
  BW.withScope $ \scope ->
  do  debug "Loading allDepArtifacts"
      style <- Reporting.terminal
      root <- getProjectRoot "allDepArtifacts"
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `lamdera make` in: " ++ root

        Right details -> allDepArtifacts_ details

{- allDepsArtifacts without Details.load, which reads/writes to disk, conflicting with
   it's use in Lamdera.Postcompile where we're already in a compile read/write phase.
 -}
allDepArtifacts_ :: Details.Details -> IO Artifacts
allDepArtifacts_ details = do
  debug "Loading allDepArtifacts_"
  root <- getProjectRoot "allDepArtifacts_"
  omvar <- Details.loadObjects root details
  imvar <- Details.loadInterfaces root details
  mdeps <- readMVar imvar
  mobjs <- readMVar omvar
  case liftM2 (,) mdeps mobjs of
    Nothing ->
      error $ "Ran into some weird problem loading elm.json\nTry running `lamdera make` in: " ++ root

    Just (deps, objs) ->
      return $ Artifacts (toInterfaces deps) objs


toInterfaces :: Map.Map ModuleName.Canonical I.DependencyInterface -> Map.Map ModuleName.Raw I.Interface
toInterfaces deps =
  Map.mapMaybe toUnique $ Map.fromListWith OneOrMore.more $
    Map.elems (Map.mapMaybeWithKey getPublic deps)


getPublic :: ModuleName.Canonical -> I.DependencyInterface -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore I.Interface)
getPublic (ModuleName.Canonical _ name) dep =
  case dep of
    I.Public  iface -> Just (name, OneOrMore.one iface)
    I.Private _ _ _ -> Nothing


toUnique :: OneOrMore.OneOrMore a -> Maybe a
toUnique oneOrMore =
  case oneOrMore of
    OneOrMore.One value -> Just value
    OneOrMore.More _ _  -> Nothing


allProjectInterfaces :: NE.List FilePath -> IO (Map.Map ModuleName.Raw I.Interface)
allProjectInterfaces paths =
  BW.withScope $ \scope -> do
    root <- getProjectRoot "allProjectInterfaces"
    let paths_ = paths & fmap (\p -> root <> "/" <> p)
    runTaskUnsafe $
      do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
          artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths_

          -- Task.io $ putStrLn $ show $ fmap (moduleName) (Build._modules artifacts)
          Task.io $ extractInterfaces $ Build._modules artifacts


runTaskUnsafe :: Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task = do
  result <- Task.run task
  case result of
    Right a ->
      return a

    Left exit ->
      do  -- Exit.toStderr (Exit.reactorToReport exit)
          error $
            "\n-------------------------------------------------\
            \\nError in allProjectInterfaces, please report this.\
            \\n-------------------------------------------------\
            \\n" ++ (exit & Exit.reactorToReport & Exit.toJson & Encode.encode & builderToString)


extractInterfaces :: [Build.Module] -> IO (Map.Map ModuleName.Raw I.Interface)
extractInterfaces modu = do
  k <- modu
    & mapM (\m ->
      case m of
        Build.Fresh nameRaw ifaces _ ->
          pure $ Just (nameRaw, ifaces)
        Build.Cached name _ mCachedInterface ->
          cachedHelp name mCachedInterface
    )
  pure $ Map.fromList $ justs k


{- Appropriated from Build.loadInterface -}
cachedHelp :: ModuleName.Raw -> MVar Build.CachedInterface -> IO (Maybe (ModuleName.Raw, I.Interface))
cachedHelp name ciMvar = do
  cachedInterface <- takeMVar ciMvar
  case cachedInterface of
    Build.Corrupted ->
      do  putMVar ciMvar cachedInterface
          return Nothing

    Build.Loaded iface ->
      do  putMVar ciMvar cachedInterface
          return (Just (name, iface))

    Build.Unneeded ->
      do  root <- getProjectRoot "cachedHelp"
          maybeIface <- File.readBinary (Stuff.elmi root name)
          case maybeIface of
            Nothing ->
              do  putMVar ciMvar Build.Corrupted
                  return Nothing

            Just iface ->
              do  putMVar ciMvar (Build.Loaded iface)
                  return (Just (name, iface))
