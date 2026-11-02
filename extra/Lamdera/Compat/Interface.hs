module Lamdera.Compat.Interface
  ( Interface(..)
  , Union(..)
  , Alias(..)
  , Binop(..)
  , toPublicUnion
  , toPublicAlias
  , extractUnion
  , extractAlias
  )
  where

{- @LAMDERA The module interface of Elm 0.19.3, kept for Lamdera's own code (see
Lamdera.Compat.Canonical for why). Lamdera.Compat.Convert.fromInterface builds one of
these from an Elm 0.19.4 Elm.Interface.Interface.

Only the data and the helpers Lamdera's code uses: these are never written to disk,
the .elmi files hold the 0.19.4 interface.
-}


import qualified Data.Map.Strict as Map

import qualified Lamdera.Compat.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Elm.Package as Pkg



-- INTERFACE


data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map N.Name Can.Annotation
    , _unions  :: Map.Map T.Name Union
    , _aliases :: Map.Map T.Name Alias
    , _binops  :: Map.Map Op.Name Binop
    }
  deriving (Eq)


data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union
  deriving (Eq)


data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias
  deriving (Eq)


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Op.Associativity
    , _op_precedence :: Op.Precedence
    }
  deriving (Eq)



-- TO PUBLIC


toPublicUnion :: Union -> Maybe Can.Union
toPublicUnion iUnion =
  case iUnion of
    OpenUnion union                       -> Just union
    ClosedUnion (Can.Union vars _ _ opts) -> Just (Can.Union vars [] 0 opts)
    PrivateUnion _                        -> Nothing


toPublicAlias :: Alias -> Maybe Can.Alias
toPublicAlias iAlias =
  case iAlias of
    PublicAlias alias -> Just alias
    PrivateAlias _    -> Nothing



-- EXTRACT


extractUnion :: Union -> Can.Union
extractUnion iUnion =
  case iUnion of
    OpenUnion union -> union
    ClosedUnion union -> union
    PrivateUnion union -> union


extractAlias :: Alias -> Can.Alias
extractAlias iAlias =
  case iAlias of
    PublicAlias alias -> alias
    PrivateAlias alias -> alias
