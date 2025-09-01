{- MANUALLY FORMATTED -}
module Compiler.Data.NonEmptyList exposing
  ( TList(..), fmap, bTList
  , singleton
  , toList
  , sortBy
  --
  , foldr
  , sequenceA
  , traverse
  )


import Extra.Class.Applicative as Applicative
import Extra.Class.Foldable as Foldable
import Extra.Class.Functor as Functor
import Extra.Class.Traversable as Traversable
import Extra.Data.Binary as B
import Extra.Type.List as MList



-- LIST


type TList a =
  CList a (MList.TList a)


singleton : a -> TList a
singleton a =
  CList a []


toList : TList a -> MList.TList a
toList (CList x xs) =
  x::xs



-- INSTANCES


fmap : Functor.Fmap a (TList a) b (TList b)
fmap func (CList x xs) = CList (func x) (MList.map func xs)


foldr : Foldable.Foldr a (TList a) b
foldr f b (CList x xs) = f x (MList.foldr f b xs)


sequenceA :
  Applicative.Pure (MList.TList a) fla
  -> Applicative.LiftA2 a fa (MList.TList a) fla (MList.TList a) fla
  -> Applicative.LiftA2 a fa (MList.TList a) fla (TList a) fta
  -> Traversable.SequenceA (TList fa) fta
sequenceA pPure pLiftA2L pLiftA2C =
  traverse pPure pLiftA2L pLiftA2C identity


traverse :
  Applicative.Pure (MList.TList b) flb
  -> Applicative.LiftA2 b fb (MList.TList b) flb (MList.TList b) flb
  -> Applicative.LiftA2 b fb (MList.TList b) flb (TList b) ftb
  -> Traversable.Traverse a (TList a) fb ftb
traverse pPure pLiftA2L pLiftA2C func (CList x xs) =
  pLiftA2C CList (func x) (MList.traverse pPure pLiftA2L func xs)



-- SORT BY


sortBy : (a -> comparable) -> TList a -> TList a
sortBy toRank (CList x xs) =
  let
    comparison a b =
      compare (toRank a) (toRank b)
  in
  case MList.sortBy comparison xs of
    [] ->
      CList x []

    y::ys ->
      case comparison x y of
        LT -> CList x (y::ys)
        EQ -> CList x (y::ys)
        GT -> CList y (MList.insertBy comparison x ys)



-- BINARY


bTList : B.Binary a -> B.Binary (TList a)
bTList binA =
  B.bin2 CList (\(CList x xs) -> B.T2 x xs) binA (B.bTList binA)
