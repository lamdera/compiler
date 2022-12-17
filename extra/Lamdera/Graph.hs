module Lamdera.Graph where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff

import Lamdera
import qualified Ext.Common


fullGraph :: [FilePath] -> IO (Either Exit.Make Opt.GlobalGraph)
fullGraph paths = do
  root <- getProjectRoot "fullGraph"
  Ext.Common.withProjectRoot root $ do
    let
      debug_ = True
      optimize = False
      maybeDocs = Nothing

    style <- Reporting.terminal

    BW.withScope $ \scope ->
      Stuff.withRootLock root $ Task.run $ do
          desiredMode <- getMode debug_ optimize
          details <- Task.eio Exit.MakeBadDetails (Details.load style scope root)
          case paths of
            [] ->
              Task.throw Exit.MakeAppNeedsFileNames
              -- The original source would try find exposed modules, but for our purposes
              -- we'll never be in the context of a package with this function, so we'll
              -- complain we have no specified source as per running `elm make` in a project
              --
              --   do  exposed <- getExposed details
              --       buildExposed style root details maybeDocs exposed
            p:ps -> do
              artifacts <- buildPaths style root details (NE.List p ps)

              case artifacts of
                (Build.Artifacts pkg _ roots modules) -> do
                  -- Task.eio Exit.MakeBadDetails $ do

                  -- @TODO this is garbaging the error with right type wrong error
                  -- need to either add an error case or remove the error Task.eio wrapping deeper in
                  Task.mapError (\err -> Exit.MakeNoOutline) $
                    getFullGraph details modules



-- Appropriated from Generate.hs

getFullGraph :: Details.Details -> [Build.Module] -> Task.Task Exit.Generate Opt.GlobalGraph
getFullGraph details modules =
  do
      root <- Task.io $ Lamdera.getProjectRoot "getFullGraph"
      objects <- finalizeObjects =<< loadObjects root details modules
      Task.io $ pure $ objectsToGlobalGraph objects


-- Cloned from Make.hs

buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task.Task Exit.Make Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths style root details paths

data DesiredMode = Debug | Dev | Prod

getMode :: Bool -> Bool -> Task.Task Exit.Make DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True , False) -> return Debug
    (False, False) -> return Dev
    (False, True ) -> return Prod


-- Cloned from Generate.hs

data Objects =
  Objects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task.Task Exit.Generate Objects
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


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
    }


loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task.Task Exit.Generate LoadingObjects
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
