{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evaluate where

import qualified Data.Map as Map

import AST.Canonical
import Elm.Package
import qualified Reporting.Annotation as A
import qualified Elm.ModuleName as Module

import Lamdera.Wire3.Helpers
import StandaloneInstances

-- import qualified Lamdera.Interfaces
import qualified Ext.Query.Canonical
import qualified Ext.Query.Interfaces
import Data.Map ((!))
import qualified Data.Map as Map


import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Data.Name
import qualified Data.Text as T
import qualified AST.Optimized as Opt

import Lamdera (formatHaskellValue, hindent_)
import qualified Lamdera.Compile
import Ext.Common
import qualified Lamdera.Evaluate.Canonical
import qualified Lamdera.Evaluate.Optimized
import System.Directory

exec root path name = do
  -- let project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter"

  -- root <- getProjectRootFor path

  withCurrentDirectory root $ track "load+exec suite" $ do

    (canonical, objectsLocal, graph) <-
      track "load" $ do
        !canonical <- Ext.Query.Canonical.loadSingleCanonical path
        !objects <- Ext.Query.Canonical.loadSingleObjects path

        !graph <- Ext.Query.Interfaces.allGraph

        pure (canonical, objects, graph)

    let
        def =
          objectsLocal
            & Opt._l_nodes
            -- & Map.lookup name
            & Map.filterWithKey (\k _ ->
                case k of
                  Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
                    name_ == name
              )
            & Map.elemAt 0
            & snd

    -- def & formatHaskellValue ("suite")

    -- formatHaskellValue "objects Test.Basic" objects
    -- formatHaskellValue "def" def
    -- formatHaskellValue "🚀" $ Lamdera.Evaluate.Optimized.test 123


    force <- track "exec" $ do
      let !force = show $ Lamdera.Evaluate.Optimized.run def Map.empty (Opt._g_nodes $ Opt.addLocalGraph objectsLocal graph)
      pure force

    formatted <- hindent_ force
    atomicPutStrLn $ T.unpack $ formatted


    -- formatHaskellValue "canonical Test.Basic" canonical
    --
    -- case canonical & _decls & findDef "suite" of
    --   Just def -> do
    --     formatHaskellValue "suite def" def
    --     formatHaskellValue "suite:" $ run def Map.empty
    --
    --   Nothing ->
    --     putStrLn "no suite found"

    pure ()
