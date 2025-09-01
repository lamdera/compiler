module Extra.Type.Set exposing
    ( Set
    , difference
    , empty
    , foldl
    , foldr
    , fromList
    , insert
    , member
    , null
    , toList
    , union
    , unions
    )

import Extra.Class.Foldable as Foldable
import Extra.Type.List as MList exposing (TList)
import Set


type alias Set comparable =
    Set.Set comparable


difference : Set comparable -> Set comparable -> Set comparable
difference =
    Set.diff


empty : Set comparable
empty =
    Set.empty


foldl : Foldable.Foldl comparable (Set comparable) a
foldl f z s =
    Set.foldl (\e acc -> f acc e) z s


foldr : Foldable.Foldr comparable (Set comparable) a
foldr =
    Set.foldr


fromList : TList comparable -> Set comparable
fromList =
    Set.fromList


insert : comparable -> Set comparable -> Set comparable
insert =
    Set.insert


member : comparable -> Set comparable -> Bool
member =
    Set.member


null : Foldable.Null (Set comparable)
null =
    Set.isEmpty


toList : Set a -> TList a
toList =
    Set.toList


union : Set comparable -> Set comparable -> Set comparable
union =
    Set.union


unions : TList (Set comparable) -> Set comparable
unions =
    MList.foldl union empty
