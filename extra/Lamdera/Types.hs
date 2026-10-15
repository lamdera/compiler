{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Types where

import qualified Data.Map as Map

import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified Elm.Interface as Interface
import qualified Elm.ModuleName as ModuleName

import Lamdera

type Interfaces =
  Map.Map Module.Name Interface.Interface


data DiffableType
  = DRecord [(Text, DiffableType)]
  | DCustom Text [(Text, [DiffableType])]
  | DString
  | DInt
  | DFloat
  | DBool
  | DOrder
  | DNever
  | DChar
  | DMaybe DiffableType
  | DList DiffableType
  | DArray DiffableType
  | DSet DiffableType
  | DResult DiffableType DiffableType
  | DDict DiffableType DiffableType
  | DTuple DiffableType DiffableType
  | DTriple DiffableType DiffableType DiffableType
  | DUnit
  | DRecursion Text
  | DKernelBrowser Text
  | DError Text
  | DExternalWarning (Text, Text, Text, Text) DiffableType
  -- Lamdera lib types special cased with support for efficiency
  | DLamderaSeqDict DiffableType DiffableType
  | DLamderaSeqSet DiffableType

  deriving (Show)


core :: [TN.Name]
core =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


type CoreTypeDiffs = [(TN.Name, String, String)]
