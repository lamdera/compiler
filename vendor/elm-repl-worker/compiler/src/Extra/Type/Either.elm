module Extra.Type.Either exposing
    ( Either(..)
    , andThen
    , bind
    , fmap
    , lefts
    , liftA2
    , pure
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.List as MList exposing (TList)


type Either a b
    = Left a
    | Right b


andThen : Monad.AndThen a (Either x a) (Either x b)
andThen callback ea =
    bind ea callback


bind : Monad.Bind a (Either x a) (Either x b)
bind ea callback =
    case ea of
        Left x ->
            Left x

        Right a ->
            callback a


fmap : Functor.Fmap a (Either x a) b (Either x b)
fmap f ea =
    case ea of
        Left x ->
            Left x

        Right a ->
            Right (f a)


lefts : TList (Either a b) -> TList a
lefts eithers =
    MList.foldr
        (\either acc ->
            case either of
                Left x ->
                    x :: acc

                Right _ ->
                    acc
        )
        []
        eithers


liftA2 : Applicative.LiftA2 a (Either x a) b (Either x b) c (Either x c)
liftA2 f ea eb =
    case ea of
        Left x ->
            Left x

        Right a ->
            case eb of
                Left x ->
                    Left x

                Right b ->
                    Right (f a b)


pure : Applicative.Pure a (Either x a)
pure =
    Right
