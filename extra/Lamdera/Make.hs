module Lamdera.Make where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Interface as I
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff


import Lamdera
import qualified Ext.Common
import qualified Ext.Query.Interfaces




-- compileToInterfaces :: FilePath -> FilePath -> IO (Map.Map ModuleName.Raw I.Interface)
compileToInterfaces :: FilePath -> FilePath -> [FilePath] -> IO (Either Exit.Make (Map.Map ModuleName.Raw I.Interface))
compileToInterfaces root path additional = do
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
          artifacts <- buildPaths style root details (NE.List path additional)

          interfaces <- Task.io $ Ext.Query.Interfaces.artifactsToFullInterfaces details artifacts

          pure interfaces


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
