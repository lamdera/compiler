module Lamdera.Make where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Interface as I
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R


import Lamdera
import qualified Ext.Common
import qualified Ext.Query.Interfaces




-- compileToInterfaces :: FilePath -> FilePath -> IO (Map.Map Module.Name I.Interface)
compileToInterfaces :: FilePath -> FilePath -> [FilePath] -> IO (Either Exit.Make (Map.Map Module.Name I.Interface))
compileToInterfaces root path additional = do
  Ext.Common.withProjectRoot root $ do
    let
      debug_ = True
      optimize = False
      -- maybeDocs = Nothing

    style <- Reporting.terminal

    R.withRootLock (R.fromFilePath root) $ \writer stuff -> Task.run $ do
          desiredMode <- getMode debug_ optimize
          details <- Task.eio Exit.MakeBadDetails (Details.load writer style (R.fromFilePath root) stuff)
          artifacts <- buildPaths writer style (R.fromFilePath root) stuff details (NE.List path additional)

          interfaces <- Task.io $ Ext.Query.Interfaces.artifactsToFullInterfaces details artifacts

          pure interfaces


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
