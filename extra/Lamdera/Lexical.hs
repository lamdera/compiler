module Lamdera.Lexical
  ( Lexical(..)
  , sortOn
  , toList
  , keys
  , elems
  , Lex(..)
  )
  where

{- Name order for everything Lamdera derives from names.

Before Elm 0.19.3 every name was a Utf8 byte array ordered by its bytes (lexically, a
prefix before its extensions). Elm 0.19.3 orders names shortest first and only then by
bytes (String.compareFast), for speed. That Ord is fine for maps that are only looked
up, but Lamdera also derives persistent and wire-visible things from name order:

  - Wire3 writes record fields in field-name order and numbers constructors in
    constructor-name order;
  - the type hashes (Lamdera.TypeHash) and Evergreen snapshots and migrations list
    fields, constructors and imports in name order.

All of those must keep the byte order of Lamdera 1.5.0 on Elm 0.19.2, so they go
through this module instead of `Ord`, `List.sortOn name` or `Map.toList` on a map keyed
by names. The order here is exactly the old one: bytes compared lexically
(String.compareLexical, the same comparison Data.Utf8's Ord made), and the old
compound orders built on it (ModuleName.Canonical: module name, then package).
-}

import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as TN
import qualified AST.Prim.TypeVar as TV
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified String as S



-- LEXICAL


class Lexical a where
  -- | The order Lamdera 1.5.0 (Elm 0.19.2) gave these values.
  lexicalCompare :: a -> a -> Ordering


instance Lexical N.Name where
  lexicalCompare x y = S.compareLexical (N.toString x) (N.toString y)

instance Lexical TN.Name where
  lexicalCompare x y = S.compareLexical (TN.nameToString x) (TN.nameToString y)

instance Lexical TV.Var where
  lexicalCompare x y = S.compareLexical (TV.varToString x) (TV.varToString y)

instance Lexical Module.Name where
  lexicalCompare x y = S.compareLexical (Module.toString x) (Module.toString y)

-- Package names were, and still are, ordered lexically (author, then project).
instance Lexical Pkg.Name where
  lexicalCompare = compare

-- Elm 0.19.2's Ord on canonical module names: module name first, then package.
instance Lexical ModuleName.Canonical where
  lexicalCompare (ModuleName.Canonical p1 n1) (ModuleName.Canonical p2 n2) =
    case lexicalCompare n1 n2 of
      EQ -> lexicalCompare p1 p2
      order -> order

instance (Lexical a, Lexical b) => Lexical (a, b) where
  lexicalCompare (a1, b1) (a2, b2) =
    case lexicalCompare a1 a2 of
      EQ -> lexicalCompare b1 b2
      order -> order



-- HELPERS


-- | List.sortOn in the old name order (stable, like List.sortOn).
sortOn :: (Lexical k) => (a -> k) -> [a] -> [a]
sortOn toKey =
  List.sortBy (\x y -> lexicalCompare (toKey x) (toKey y))


-- | Map.toList in the old name order.
toList :: (Lexical k) => Map.Map k v -> [(k, v)]
toList =
  sortOn fst . Map.toList


keys :: (Lexical k) => Map.Map k v -> [k]
keys =
  fmap fst . toList


elems :: (Lexical k) => Map.Map k v -> [v]
elems =
  fmap snd . toList



-- KEYS
--
-- Map and Set keys ordered the old way, for collections whose iteration order shows.


newtype Lex a = Lex { unLex :: a }


instance (Lexical a) => Eq (Lex a) where
  (==) (Lex x) (Lex y) =
    lexicalCompare x y == EQ

instance (Lexical a) => Ord (Lex a) where
  compare (Lex x) (Lex y) =
    lexicalCompare x y
