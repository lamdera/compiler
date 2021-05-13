module Lamdera.Types where

import Lamdera

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
  deriving (Show)
