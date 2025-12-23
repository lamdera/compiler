module Extra.Type.List exposing
    ( TList
    , all
    , andMap
    , any
    , concat
    , concatMap
    , drop
    , elem
    , filter
    , filterM
    , foldl
    , foldl1
    , foldlM
    , foldr
    , foldr1
    , head
    , indexedFrom
    , init
    , insertBy
    , intersperse
    , last
    , length
    , lookup
    , map
    , mapM_
    , mapMaybe
    , mappend
    , maximum
    , notelem
    , null
    , pure
    , range
    , replicate
    , reverse
    , sequenceA
    , sortBy
    , sortOn
    , splitAt
    , take
    , traverse
    , unzip
    , unzip3
    , zip
    , zipWith
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Foldable as Foldable
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.Monoid as Monoid
import Extra.Class.Traversable as Traversable


type alias TList a =
    List a


all : Foldable.All a (TList a)
all =
    List.all


andMap : Applicative.AndMap (TList a) (TList (a -> b)) (TList b)
andMap la lf =
    concatMap (\f -> map f la) lf


any : (a -> Bool) -> TList a -> Bool
any =
    List.any


concat : TList (TList a) -> TList a
concat =
    List.concat


concatMap : (a -> TList b) -> TList a -> TList b
concatMap =
    List.concatMap


drop : Int -> TList a -> TList a
drop =
    List.drop


elem : Foldable.Elem a (TList a)
elem =
    List.member


filter : Foldable.Filter a (TList a)
filter =
    List.filter


filterM :
    Applicative.Pure (TList a) mla
    -> Applicative.LiftA2 Bool mb (TList a) mla (TList a) mla
    -> ((a -> mb) -> TList a -> mla)
filterM pPure pLiftA2 predicate =
    foldr
        (\x ->
            pLiftA2
                (\flag ->
                    if flag then
                        \l -> x :: l

                    else
                        identity
                )
                (predicate x)
        )
        (pPure [])


foldl : Foldable.Foldl a (TList a) b
foldl f z l =
    List.foldl (\a b -> f b a) z l


foldl1 : (a -> a -> a) -> TList a -> a
foldl1 f xs =
    let
        mf m y =
            Just
                (case m of
                    Nothing ->
                        y

                    Just x ->
                        f x y
                )
    in
    case foldl mf Nothing xs of
        Nothing ->
            Debug.todo "Extra.Type.List_.foldl1: empty list"

        Just x ->
            x


foldlM :
    Monad.Return b mb
    -> Monad.Bind b mb mb
    -> Foldable.FoldlM a (TList a) b mb
foldlM pReturn pBind f z l =
    Foldable.foldlM foldr pReturn pBind f z l


foldr : Foldable.Foldr a (TList a) b
foldr =
    List.foldr


foldr1 : (a -> a -> a) -> TList a -> a
foldr1 f xs =
    let
        mf : a -> Maybe a -> Maybe a
        mf x acc =
            Just
                (case acc of
                    Nothing ->
                        x

                    Just y ->
                        f x y
                )
    in
    case foldr mf Nothing xs of
        Nothing ->
            Debug.todo "Extra.Type.List_.foldr1: empty list"

        Just x ->
            x


head : TList a -> a
head l =
    case l of
        h :: _ ->
            h

        [] ->
            Debug.todo "Extra.Type.List_.head: empty list"


indexedFrom : Int -> TList a -> TList ( Int, a )
indexedFrom n l =
    List.indexedMap (\i a -> ( i + n, a )) l


init : TList a -> TList a
init =
    reverse >> drop 1 >> reverse


insertBy : (a -> a -> Order) -> a -> TList a -> TList a
insertBy cmp x ys =
    case ys of
        [] ->
            [ x ]

        y :: ys_ ->
            case cmp x y of
                GT ->
                    y :: insertBy cmp x ys_

                _ ->
                    x :: ys


intersperse : a -> TList a -> TList a
intersperse =
    List.intersperse


last : TList a -> a
last l =
    head (reverse l)


length : Foldable.Length (TList a)
length =
    List.length


lookup : a -> TList ( a, b ) -> Maybe b
lookup key l =
    case l of
        [] ->
            Nothing

        ( k, b ) :: rest ->
            if key == k then
                Just b

            else
                lookup key rest


map : Functor.Fmap a (TList a) b (TList b)
map =
    List.map


mapM_ :
    Monad.Return () mu
    -> Monad.Bind b mb mu
    -> Foldable.MapM_ a (TList a) mb mu
mapM_ pReturn pBind f l =
    Foldable.mapM_ foldr pReturn pBind f l


mapMaybe : (a -> Maybe b) -> TList a -> TList b
mapMaybe =
    List.filterMap


mappend : Monoid.Mappend (TList a)
mappend =
    List.append


maximum : TList comparable -> comparable
maximum l =
    case List.maximum l of
        Just x ->
            x

        Nothing ->
            Debug.todo "Extra.Type.List.maximum: empty list"


notelem : Foldable.NotElem a (TList a)
notelem a l =
    not (elem a l)


null : Foldable.Null (TList a)
null =
    List.isEmpty


pure : Applicative.Pure a (TList a)
pure =
    List.singleton


range : Int -> Int -> TList Int
range =
    List.range


replicate : Int -> a -> TList a
replicate =
    List.repeat


reverse : TList a -> TList a
reverse =
    List.reverse


sequenceA :
    Applicative.Pure (TList a) fla
    -> Applicative.LiftA2 a fa (TList a) fla (TList a) fla
    -> Traversable.SequenceA (TList fa) fla
sequenceA pPure pListA2 l =
    Traversable.sequenceA (traverse pPure pListA2) l


sortBy : (a -> a -> Order) -> TList a -> TList a
sortBy =
    List.sortWith


sortOn : (a -> comparable) -> TList a -> TList a
sortOn =
    List.sortBy


splitAt : Int -> TList a -> ( TList a, TList a )
splitAt n l =
    ( take n l, drop n l )


take : Int -> TList a -> TList a
take =
    List.take


traverse :
    Applicative.Pure (TList b) flb
    -> Applicative.LiftA2 b fb (TList b) flb (TList b) flb
    -> Traversable.Traverse a (TList a) fb flb
traverse pPure pLiftA2 f l =
    foldr (\a flb -> pLiftA2 (::) (f a) flb) (pPure []) l


unzip : TList ( a, b ) -> ( TList a, TList b )
unzip =
    List.unzip


unzip3 : TList ( a, b, c ) -> ( TList a, TList b, TList c )
unzip3 =
    -- Inline so that fusion `foldr` has an opportunity to fire.
    -- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
    foldr (\( a, b, c ) ( la, lb, lc ) -> ( a :: la, b :: lb, c :: lc ))
        ( [], [], [] )


zip : TList a -> TList b -> TList ( a, b )
zip la lb =
    zipWith Tuple.pair la lb


zipWith : (a -> b -> c) -> TList a -> TList b -> TList c
zipWith =
    List.map2
