module Extra.System.IO.Pure exposing
    ( IO
    , Step(..)
    , andMap
    , andThen
    , bind
    , fmap
    , foldlMList
    , foldlMMap
    , foldrMList
    , forMList_
    , imapMList_
    , liftA2
    , liftM
    , liftM2
    , liftM3
    , liftS
    , loop
    , mapMList_
    , mapMMap_
    , performIO
    , pure
    , return
    , traverseList
    , traverseMap
    , traverseWithKey
    , when
    )

-- pure IO implemented as the State monad with an explicit state type

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map


type alias IO s a =
    s -> ( a, s )


andMap : Applicative.AndMap (IO s a) (IO s (a -> b)) (IO s b)
andMap =
    Applicative.andMap liftA2


andThen : Monad.AndThen a (IO s a) (IO s b)
andThen =
    Monad.andThen bind


bind : Monad.Bind a (IO s a) (IO s b)
bind ma callback s0 =
    let
        ( a, s1 ) =
            ma s0
    in
    callback a s1


fmap : Functor.Fmap a (IO s a) b (IO s b)
fmap f ma s0 =
    let
        ( a, s1 ) =
            ma s0
    in
    ( f a, s1 )


liftA2 : Applicative.LiftA2 a (IO s a) b (IO s b) c (IO s c)
liftA2 f ma mb s0 =
    let
        ( a, s1 ) =
            ma s0

        ( b, s2 ) =
            mb s1
    in
    ( f a b, s2 )


liftM : (a -> b) -> IO s a -> IO s b
liftM f ma =
    bind ma (\a -> return (f a))


liftM2 : (a -> b -> c) -> IO s a -> IO s b -> IO s c
liftM2 f ma mb =
    bind ma (\a -> bind mb (\b -> return (f a b)))


liftM3 : (a -> b -> c -> d) -> IO s a -> IO s b -> IO s c -> IO s d
liftM3 f ma mb mc =
    bind ma (\a -> bind mb (\b -> bind mc (\c -> return (f a b c))))


liftS : (s2 -> s1) -> (s1 -> s2 -> s2) -> IO s1 a -> IO s2 a
liftS getter setter state1 s2 =
    let
        ( a, s1 ) =
            state1 (getter s2)
    in
    ( a, setter s1 s2 )


performIO : IO s a -> s -> a
performIO ma s =
    Tuple.first (ma s)


pure : Applicative.Pure a (IO s a)
pure a s =
    ( a, s )


return : Monad.Return a (IO s a)
return =
    pure


when : Applicative.When (IO s ())
when =
    Applicative.when pure



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : (state -> IO s (Step state a)) -> state -> IO s a
loop callback loopState ioState =
    case callback loopState ioState of
        ( Loop newLoopState, newIOState ) ->
            loop callback newLoopState newIOState

        ( Done a, newIOState ) ->
            ( a, newIOState )



-- FOLD AND TRAVERSAL


foldlMList : (b -> a -> IO s b) -> b -> TList a -> IO s b
foldlMList callback zero list =
    loop (foldlMHelpList callback) ( list, zero )


foldlMHelpList : (b -> a -> IO s b) -> ( TList a, b ) -> IO s (Step ( TList a, b ) b)
foldlMHelpList callback ( list, result ) =
    case list of
        [] ->
            return (Done result)

        a :: rest ->
            fmap (\b -> Loop ( rest, b )) (callback result a)


foldrMList : (a -> b -> IO s b) -> b -> TList a -> IO s b
foldrMList callback zero list =
    loop (foldrMHelpList callback) ( MList.reverse list, zero )


foldrMHelpList : (a -> b -> IO s b) -> ( TList a, b ) -> IO s (Step ( TList a, b ) b)
foldrMHelpList callback ( list, result ) =
    case list of
        [] ->
            return (Done result)

        a :: rest ->
            fmap (\b -> Loop ( rest, b )) (callback a result)


forMList_ : TList a -> (a -> IO s b) -> IO s ()
forMList_ list callback =
    loop (mapMListHelp_ callback) ( list, pure () )


imapMList_ : (Int -> a -> IO s b) -> TList a -> IO s ()
imapMList_ callback list =
    loop (imapMListHelp_ callback) ( MList.reverse list, MList.length list - 1, pure () )


imapMListHelp_ : (Int -> a -> IO s b) -> ( TList a, Int, IO s () ) -> IO s (Step ( TList a, Int, IO s () ) ())
imapMListHelp_ callback ( list, index, result ) =
    case list of
        [] ->
            fmap Done result

        a :: rest ->
            return (Loop ( rest, index - 1, bind (callback index a) (\_ -> result) ))


mapMList_ : (a -> IO s b) -> TList a -> IO s ()
mapMList_ callback list =
    loop (mapMListHelp_ callback) ( list, pure () )


mapMListHelp_ : (a -> IO s b) -> ( TList a, IO s () ) -> IO s (Step ( TList a, IO s () ) ())
mapMListHelp_ callback ( list, result ) =
    case list of
        [] ->
            fmap Done result

        a :: rest ->
            fmap (\_ -> Loop ( rest, result )) (callback a)


traverseList : (a -> IO s b) -> TList a -> IO s (TList b)
traverseList callback list =
    loop (traverseListHelp callback) ( list, [] )


traverseListHelp : (a -> IO s b) -> ( TList a, TList b ) -> IO s (Step ( TList a, TList b ) (TList b))
traverseListHelp callback ( list, result ) =
    case list of
        [] ->
            return (Done (List.reverse result))

        a :: rest ->
            fmap (\b -> Loop ( rest, b :: result )) (callback a)


foldlMMap : (b -> a -> IO s b) -> b -> Map.Map comparable a -> IO s b
foldlMMap callback zero map =
    loop (foldlMHelpMap callback) ( Map.toList map, zero )


foldlMHelpMap : (b -> a -> IO s b) -> ( TList ( comparable, a ), b ) -> IO s (Step ( TList ( comparable, a ), b ) b)
foldlMHelpMap callback ( pairs, result ) =
    case pairs of
        [] ->
            return (Done result)

        ( _, a ) :: rest ->
            fmap (\b -> Loop ( rest, b )) (callback result a)


mapMMap_ : (a -> IO s b) -> Map.Map comparable a -> IO s ()
mapMMap_ callback map =
    loop (mapMMapHelp_ callback) ( Map.toList map, pure () )


mapMMapHelp_ : (a -> IO s b) -> ( TList ( comparable, a ), IO s () ) -> IO s (Step ( TList ( comparable, a ), IO s () ) ())
mapMMapHelp_ callback ( list, result ) =
    case list of
        [] ->
            fmap Done result

        ( _, a ) :: rest ->
            fmap (\_ -> Loop ( rest, result )) (callback a)


traverseMap : (a -> IO s b) -> Map.Map comparable a -> IO s (Map.Map comparable b)
traverseMap callback map =
    loop (traverseMapHelp callback) ( Map.toList map, Map.empty )


traverseMapHelp : (a -> IO s b) -> ( TList ( comparable, a ), Map.Map comparable b ) -> IO s (Step ( TList ( comparable, a ), Map.Map comparable b ) (Map.Map comparable b))
traverseMapHelp callback ( pairs, result ) =
    case pairs of
        [] ->
            return (Done result)

        ( k, a ) :: rest ->
            fmap (\b -> Loop ( rest, Map.insert k b result )) (callback a)


traverseWithKey : (comparable -> a -> IO s b) -> Map.Map comparable a -> IO s (Map.Map comparable b)
traverseWithKey callback map =
    loop (traverseWithKeyHelp callback) ( Map.toList map, Map.empty )


traverseWithKeyHelp : (comparable -> a -> IO s b) -> ( TList ( comparable, a ), Map.Map comparable b ) -> IO s (Step ( TList ( comparable, a ), Map.Map comparable b ) (Map.Map comparable b))
traverseWithKeyHelp callback ( pairs, result ) =
    case pairs of
        [] ->
            return (Done result)

        ( k, a ) :: rest ->
            fmap (\b -> Loop ( rest, Map.insert k b result )) (callback k a)
