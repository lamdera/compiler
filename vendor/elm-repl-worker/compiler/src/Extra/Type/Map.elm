module Extra.Type.Map exposing
    ( Map
    , alter
    , any
    , delete
    , difference
    , dropMissing
    , elems
    , empty
    , ex
    , filter
    , findMin
    , findWithDefault
    , foldl
    , foldlWithKey
    , foldr
    , foldrWithKey
    , fromKeys
    , fromKeysA
    , fromList
    , fromListWith
    , insert
    , insertWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    , keys
    , keysSet
    , lookup
    , lookupMin
    , map
    , mapKeys
    , mapM_
    , mapMaybe
    , mapMaybeMissing
    , mapMaybeWithKey
    , mapMissing
    , mapWithKey
    , member
    , mergeA
    , minViewWithKey
    , null
    , preserveMissing
    , sequenceA
    , sequence_
    , singleton
    , size
    , toList
    , traverse
    , traverseMaybeWithKey
    , traverseWithKey
    , traverse_
    , union
    , unionWith
    , unions
    , unionsWith
    , zipWithAMatched
    , zipWithMatched
    , zipWithMaybeMatched
    )

import Dict exposing (Dict)
import Extra.Class.Applicative as Applicative
import Extra.Class.Foldable as Foldable
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.Traversable as Traversable
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Set as Set exposing (Set)


type alias Map comparable a =
    Dict comparable a


alter : (Maybe a -> Maybe a) -> comparable -> Map comparable a -> Map comparable a
alter f k m =
    Dict.update k f m


any : (a -> Bool) -> Map comparable a -> Bool
any p m =
    foldl (\acc a -> acc || p a) False m


delete : comparable -> Map comparable a -> Map comparable a
delete =
    Dict.remove


difference : Map comparable a -> Map comparable b -> Map comparable a
difference =
    Dict.diff


dropMissing :
    Applicative.Pure (Maybe c) fmc
    -> (comparable -> a -> fmc)
dropMissing pPure _ _ =
    pPure Nothing


elems : Map comparable a -> TList a
elems =
    Dict.values


empty : Map comparable a
empty =
    Dict.empty


ex : Map comparable a -> comparable -> a
ex m k =
    -- (!)
    case lookup k m of
        Just a ->
            a

        Nothing ->
            Debug.todo <| "Map.ex: key " ++ Debug.toString k ++ " not found"


filter : (a -> Bool) -> Map comparable a -> Map comparable a
filter p m =
    Dict.filter (\_ a -> p a) m


findMin : Map comparable a -> ( comparable, a )
findMin m =
    case lookupMin m of
        Just ka ->
            ka

        Nothing ->
            Debug.todo "Map.findMin: empty map has no minimal element"


findWithDefault : a -> comparable -> Map comparable a -> a
findWithDefault def k m =
    lookup k m
        |> Maybe.withDefault def


foldl : Foldable.Foldl b (Map comparable b) a
foldl f z m =
    foldlWithKey (\a _ b -> f a b) z m


foldlWithKey : (a -> comparable -> b -> a) -> a -> Map comparable b -> a
foldlWithKey f z m =
    Dict.foldl (\k b a -> f a k b) z m


foldr : Foldable.Foldr a (Map comparable a) b
foldr f z m =
    foldrWithKey (\_ -> f) z m


foldrWithKey : (comparable -> a -> b -> b) -> b -> Map comparable a -> b
foldrWithKey =
    Dict.foldr


fromKeys : (comparable -> a) -> TList comparable -> Map comparable a
fromKeys f l =
    MList.foldl (\acc k -> insert k (f k) acc) empty l


fromKeysA :
    Applicative.Pure (Map comparable a) fda
    -> Applicative.LiftA2 (Map comparable a) fda a fa (Map comparable a) fda
    -> ((comparable -> fa) -> TList comparable -> fda)
fromKeysA pPure pLiftA2 f l =
    MList.foldl (\acc k -> pLiftA2 (\da a -> insert k a da) acc (f k)) (pPure empty) l


fromList : TList ( comparable, a ) -> Map comparable a
fromList =
    Dict.fromList


fromListWith : (a -> a -> a) -> TList ( comparable, a ) -> Map comparable a
fromListWith f l =
    MList.foldl (\m ( k, a ) -> insertWith f k a m) empty l


insert : comparable -> a -> Map comparable a -> Map comparable a
insert =
    Dict.insert


insertWith : (a -> a -> a) -> comparable -> a -> Map comparable a -> Map comparable a
insertWith f k new m =
    Dict.update k
        (\ma ->
            Just <|
                case ma of
                    Just old ->
                        f new old

                    Nothing ->
                        new
        )
        m


intersection : Map comparable a -> Map comparable b -> Map comparable a
intersection =
    intersectionWith (\a _ -> a)


intersectionWith : (a -> b -> c) -> Map comparable a -> Map comparable b -> Map comparable c
intersectionWith f m1 m2 =
    intersectionWithKey (\_ -> f) m1 m2


intersectionWithKey : (comparable -> a -> b -> c) -> Map comparable a -> Map comparable b -> Map comparable c
intersectionWithKey f m1 m2 =
    Dict.merge
        (\_ _ m -> m)
        (\k a1 a2 -> insert k (f k a1 a2))
        (\_ _ m -> m)
        m1
        m2
        empty


keys : Map k v -> TList k
keys =
    Dict.keys


keysSet : Map comparable a -> Set comparable
keysSet m =
    foldrWithKey (\k _ s -> Set.insert k s) Set.empty m


lookup : comparable -> Map comparable a -> Maybe a
lookup =
    Dict.get


lookupMin : Map comparable a -> Maybe ( comparable, a )
lookupMin m =
    foldlWithKey
        (\mka k a ->
            case mka of
                Nothing ->
                    Just ( k, a )

                _ ->
                    mka
        )
        Nothing
        m


map : Functor.Fmap a (Map comparable a) b (Map comparable b)
map f m =
    mapWithKey (\_ -> f) m


mapKeys : (comparable1 -> comparable2) -> Map comparable1 a -> Map comparable2 a
mapKeys f m =
    foldlWithKey (\acc k a -> insert (f k) a acc) empty m


mapM_ :
    Monad.Return () mu
    -> Monad.Bind b mb mu
    -> Foldable.MapM_ a (Map comparable a) mb mu
mapM_ pReturn pBind f m =
    Foldable.mapM_ foldr pReturn pBind f m


mapMaybe : (a -> Maybe b) -> Map comparable a -> Map comparable b
mapMaybe f m =
    mapMaybeWithKey (\_ -> f) m


mapMaybeMissing :
    Applicative.Pure (Maybe c) fmc
    -> (comparable -> a -> Maybe c)
    -> (comparable -> a -> fmc)
mapMaybeMissing pPure f k a =
    pPure (f k a)


mapMaybeWithKey : (comparable -> a -> Maybe b) -> Map comparable a -> Map comparable b
mapMaybeWithKey f m =
    foldrWithKey
        (\k a new ->
            case f k a of
                Just b ->
                    insert k b new

                Nothing ->
                    new
        )
        empty
        m


mapMissing :
    Applicative.Pure (Maybe c) fmc
    -> (comparable -> a -> c)
    -> (comparable -> a -> fmc)
mapMissing pPure f k a =
    pPure (Just (f k a))


mapWithKey : (k -> a -> b) -> Map k a -> Map k b
mapWithKey =
    Dict.map


member : comparable -> Map comparable a -> Bool
member =
    Dict.member


mergeA :
    Applicative.Pure (Map comparable c) fdc
    -> Applicative.LiftA2 (Maybe c) fmc (Map comparable c) fdc (Map comparable c) fdc
    -> (comparable -> a -> fmc)
    -> (comparable -> b -> fmc)
    -> (comparable -> a -> b -> fmc)
    -> Map comparable a
    -> Map comparable b
    -> fdc
mergeA pPure pLiftA2 missA missB zipAB ma mb =
    Dict.merge
        (toSingleStep pLiftA2 missA)
        (toBothStep pLiftA2 zipAB)
        (toSingleStep pLiftA2 missB)
        ma
        mb
        (pPure empty)


minViewWithKey : Map comparable a -> Maybe ( ( comparable, a ), Map comparable a )
minViewWithKey m =
    lookupMin m |> Maybe.map (\( k, a ) -> ( ( k, a ), Dict.remove k m ))


null : Foldable.Null (Map comparable a)
null =
    Dict.isEmpty


preserveMissing :
    Applicative.Pure (Maybe a) fma
    -> (comparable -> a -> fma)
preserveMissing pPure _ a =
    pPure (Just a)


sequence_ :
    Monad.Return () mu
    -> Monad.Bind a ma mu
    -> Foldable.Sequence_ (Map comparable ma) mu
sequence_ pReturn pBind =
    Foldable.sequence_ foldr pReturn pBind


sequenceA :
    Applicative.Pure (Map comparable a) fda
    -> Applicative.LiftA2 a fa (Map comparable a) fda (Map comparable a) fda
    -> Traversable.SequenceA (Map comparable fa) fda
sequenceA pPure pLiftA2 =
    Traversable.sequenceA (traverse pPure pLiftA2)


singleton : comparable -> a -> Map comparable a
singleton =
    Dict.singleton


size : Map comparable a -> Int
size =
    Dict.size


toBothStep :
    Applicative.LiftA2 (Maybe c) fmc (Map comparable c) fdc (Map comparable c) fdc
    -> (comparable -> a -> b -> fmc)
    -> (comparable -> a -> b -> fdc -> fdc)
toBothStep pLiftA2 f k a b fdc =
    pLiftA2
        (\mc dc ->
            case mc of
                Just c ->
                    insert k c dc

                Nothing ->
                    dc
        )
        (f k a b)
        fdc


toList : Map comparable a -> TList ( comparable, a )
toList =
    Dict.toList


toSingleStep :
    Applicative.LiftA2 (Maybe c) fmc (Map comparable c) fdc (Map comparable c) fdc
    -> (comparable -> a -> fmc)
    -> (comparable -> a -> fdc -> fdc)
toSingleStep pLiftA2 f k a fdc =
    pLiftA2
        (\mc dc ->
            case mc of
                Just c ->
                    insert k c dc

                Nothing ->
                    dc
        )
        (f k a)
        fdc


traverse :
    Applicative.Pure (Map comparable b) fdb
    -> Applicative.LiftA2 b fb (Map comparable b) fdb (Map comparable b) fdb
    -> Traversable.Traverse a (Map comparable a) fb fdb
traverse pPure pLiftA2 f =
    traverseWithKey pPure pLiftA2 (\_ -> f)


traverse_ :
    Applicative.Pure () fu
    -> Applicative.LiftA2 b fb () fu () fu
    -> Foldable.Traverse_ a (Map comparable a) fb fu
traverse_ pPure pLiftA2 f t =
    Foldable.traverse_ pPure pLiftA2 foldr f t


traverseMaybeWithKey :
    Applicative.Pure (Map comparable b) fdb
    -> Applicative.LiftA2 (Maybe b) fmb (Map comparable b) fdb (Map comparable b) fdb
    -> ((comparable -> a -> fmb) -> Map comparable a -> fdb)
traverseMaybeWithKey pPure pLiftA2 f dict =
    foldrWithKey
        (\k a fdb ->
            pLiftA2
                (\mb db ->
                    case mb of
                        Just b ->
                            insert k b db

                        Nothing ->
                            db
                )
                (f k a)
                fdb
        )
        (pPure empty)
        dict


traverseWithKey :
    Applicative.Pure (Map comparable b) fdb
    -> Applicative.LiftA2 b fb (Map comparable b) fdb (Map comparable b) fdb
    -> ((comparable -> a -> fb) -> Map comparable a -> fdb)
traverseWithKey pPure pLiftA2 f ma =
    foldrWithKey
        (\k a fdb ->
            pLiftA2 (\b db -> insert k b db) (f k a) fdb
        )
        (pPure empty)
        ma


union : Map comparable a -> Map comparable a -> Map comparable a
union =
    Dict.union


unions :
    Foldable.Foldl (Map comparable a) ta (Map comparable a)
    -> (ta -> Map comparable a)
unions pFoldl t =
    pFoldl union empty t


unionWith : (a -> a -> a) -> Map comparable a -> Map comparable a -> Map comparable a
unionWith f m1 m2 =
    Dict.merge
        (\k a1 -> insert k a1)
        (\key a1 a2 -> insert key (f a1 a2))
        (\key a2 -> insert key a2)
        m1
        m2
        empty


unionsWith :
    Foldable.Foldl (Map comparable a) tma (Map comparable a)
    -> ((a -> a -> a) -> tma -> Map comparable a)
unionsWith pFoldl f t =
    pFoldl (unionWith f) empty t


zipWithAMatched :
    Functor.Fmap c fc (Maybe c) fmc
    -> (comparable -> a -> b -> fc)
    -> (comparable -> a -> b -> fmc)
zipWithAMatched pFmap f k a b =
    pFmap Just (f k a b)


zipWithMatched :
    Applicative.Pure (Maybe c) fmc
    -> (comparable -> a -> b -> c)
    -> (comparable -> a -> b -> fmc)
zipWithMatched pPure f k a b =
    pPure (Just (f k a b))


zipWithMaybeMatched :
    Applicative.Pure (Maybe c) fmc
    -> (comparable -> a -> b -> Maybe c)
    -> (comparable -> a -> b -> fmc)
zipWithMaybeMatched pPure f k a b =
    pPure (f k a b)
