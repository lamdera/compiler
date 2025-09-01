module Extra.Type.Maybe exposing
    ( bind
    , catMaybes
    , isJust
    , mapMaybe
    , maybe
    , maybeToList
    , sequenceA
    , traverse
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Traversable as Traversable
import Extra.Type.List exposing (TList)


bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind ma f =
    Maybe.andThen f ma


catMaybes : TList (Maybe a) -> TList a
catMaybes =
    mapMaybe identity


isJust : Maybe a -> Bool
isJust l =
    l /= Nothing


mapMaybe : (a -> Maybe b) -> TList a -> TList b
mapMaybe f l =
    case l of
        [] ->
            []

        x :: xs ->
            let
                rs =
                    mapMaybe f xs
            in
            case f x of
                Nothing ->
                    rs

                Just r ->
                    r :: rs


maybe : b -> (a -> b) -> Maybe a -> b
maybe b f ma =
    case ma of
        Just a ->
            f a

        Nothing ->
            b


maybeToList : Maybe a -> TList a
maybeToList l =
    case l of
        Nothing ->
            []

        Just x ->
            [ x ]


sequenceA :
    Applicative.Pure (Maybe a) fma
    -> Functor.Fmap a fa (Maybe a) fma
    -> Traversable.SequenceA (Maybe fa) fma
sequenceA pPure pFmap mfa =
    Traversable.sequenceA (traverse pPure pFmap) mfa


traverse :
    Applicative.Pure (Maybe b) fmb
    -> Functor.Fmap b fb (Maybe b) fmb
    -> Traversable.Traverse a (Maybe a) fb fmb
traverse pPure pFmap f ma =
    case ma of
        Just a ->
            pFmap Just (f a)

        Nothing ->
            pPure Nothing
