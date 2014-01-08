{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Literal where

import Data.Map (Map)
import SourceSyntax.PrettyPrint
import qualified Text.PrettyPrint as PP

data Literal = IntNum Int
             | FloatNum Double
             | Chr Char
             | Str String
             | Boolean Bool
             deriving (Eq, Ord, Show)

instance Pretty Literal where
  pretty literal =
    case literal of
      IntNum n -> PP.int n
      FloatNum n -> PP.double n
      Chr c -> PP.quotes (PP.char c)
      Str s -> PP.text (show s)
      Boolean bool -> PP.text (show bool)

data GLTipe = V3 | M4

glTipeName :: GLTipe -> String
glTipeName V3 = "MJS.V3"
glTipeName M4 = "MJS.M4x4"

data GLShaderTipe = GLShaderTipe
    { attribute :: Map String GLTipe
    , uniform :: Map String GLTipe
    , varying :: Map String GLTipe
    }

