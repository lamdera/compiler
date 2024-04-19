{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Types where

import qualified Data.Map as Map

import qualified Data.Name as N
import qualified Elm.Interface as Interface
import qualified Elm.ModuleName as ModuleName

import Lamdera

type Interfaces =
  Map.Map ModuleName.Raw Interface.Interface


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
  | DLamderaHashMapDict DiffableType DiffableType
  | DLamderaHashMapSet DiffableType

  deriving (Show)


core :: [ModuleName.Raw]
core =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


type CoreTypeDiffs = [(N.Name, String, String)]
