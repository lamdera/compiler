module Lamdera.Graph where

import Control.Monad (liftM2)
import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R
import qualified ThreadSafe.Fork as Fork

import Lamdera
import qualified Ext.Common


fullGraph :: [FilePath] -> IO (Either Exit.Make Opt.GlobalGraph)
fullGraph paths = do
  root <- getProjectRoot "fullGraph"
  Ext.Common.withProjectRoot root $ do
    let
      debug_ = True
      optimize = False
      -- maybeDocs = Nothing

    style <- Reporting.terminal

    R.withRootLock (R.fromFilePath root) $ \writer stuff -> Task.run $ do
          desiredMode <- getMode debug_ optimize
          details <- Task.eio Exit.MakeBadDetails (Details.load writer style (R.fromFilePath root) stuff)
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
              artifacts <- buildPaths writer style (R.fromFilePath root) stuff details (NE.List p ps)

              case artifacts of
                (Build.Artifacts pkg _ roots modules) -> do
                  -- Task.eio Exit.MakeBadDetails $ do

                  -- @TODO this is garbaging the error with right type wrong error
                  -- need to either add an error case or remove the error Task.eio wrapping deeper in
                  Task.mapError (\err -> Exit.MakeNoOutline) $
                    getFullGraph stuff details modules



-- Appropriated from Generate.hs

getFullGraph :: R.Stuff -> Details.Details -> [Build.Module] -> Task.Task Exit.Generate Opt.GlobalGraph
getFullGraph stuff details modules =
  do
      objects <- finalizeObjects =<< loadObjects stuff details modules
      Task.io $ pure $ objectsToGlobalGraph objects


-- Cloned from Make.hs

buildPaths :: File.Writer R.PROJECT -> Reporting.Style -> R.Root -> R.Stuff -> Details.Details -> NE.List FilePath -> Task.Task Exit.Make Build.Artifacts
buildPaths writer style root stuff details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths writer style root stuff details paths

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
    , _locals :: Map.Map Module.Name Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task.Task Exit.Generate Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- Fork.await mvar
      results <- traverse Fork.await mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: Fork.SafeMVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map Module.Name (Fork.SafeMVar (Maybe Opt.LocalGraph))
    }


loadObjects :: R.Stuff -> Details.Details -> [Build.Module] -> Task.Task Exit.Generate LoadingObjects
loadObjects stuff details modules =
  Task.io $
  do  mvar <- Details.loadObjects stuff details
      mvars <- traverse (loadObject stuff) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: R.Stuff -> Build.Module -> IO (Module.Name, Fork.SafeMVar (Maybe Opt.LocalGraph))
loadObject stuff modul =
  case modul of
    Build.Fresh  name _ graph -> (,) name <$> Fork.cached (Just graph)
    Build.Cached name _ _     -> (,) name <$> Fork.fork name (File.readBytes Opt.dLocalGraph (R.elmo stuff name))
