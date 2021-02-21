{-# LANGUAGE OverloadedStrings #-}

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
import Data.Map ((!))
import qualified Data.Map as Map


import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Data.Name
import qualified Data.Text as T
import qualified AST.Optimized as Opt

import Lamdera (formatHaskellValue)
import qualified Lamdera.Compile
import Ext.Common
import qualified Lamdera.Evaluate.Canonical
import qualified Lamdera.Evaluate.Optimized
import System.Directory

exec = do
  let project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter"
  withCurrentDirectory project $ track "load+exec suite" $ do

    canonical <- Ext.Query.Canonical.loadSingleCanonical "src/Test/Basic.elm"
    objects <- Ext.Query.Canonical.loadSingleObjects "src/Test/Basic.elm"

    -- objects
    --   & _l_nodes
    --   &

    let name = "suite"


        def =
          objects
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

    formatHaskellValue "🚀" $ Lamdera.Evaluate.Optimized.run def Map.empty (objects & Opt._l_nodes)


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
