{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.ElmPages.Check where

import qualified Data.Map as Map

import qualified Reporting.Error as E
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Lamdera
import Lamdera.Types
import qualified Ext.TypeHash
import StandaloneInstances


isWireCompatible :: Pkg.Name -> ModuleName.Raw -> ModuleName.Raw -> Can.Module -> Interfaces -> Bool -> Either E.Error ()
isWireCompatible pkg moduleName target canonical ifaces inDebug = do
  case Ext.TypeHash.calculateHashes pkg moduleName [target] canonical ifaces inDebug of
    Right _ -> Right ()

    Left err ->
      case err of
        Exit.BuildLamderaProblem title topline ddoc ->
          Left $ E.BadLamderaWireIncompatible $ Help.reportToDoc $ Help.report title Nothing topline ddoc

        _ -> error "todo: remove the impossible states"
