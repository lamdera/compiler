module Extra.Data.Binary.Get exposing
    ( ByteOffset
    , Get
    , bind
    , fail
    , fmap
    , getSequence
    , getString
    , getWord16
    , getWord32
    , getWord8
    , liftM2
    , liftM3
    , liftM4
    , liftM5
    , liftM6
    , return
    , runGetOrFail
    )

import Bytes
import Bytes.Decode
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.Either as Either exposing (Either(..))


type alias Get a =
    ByteOffset -> Bytes.Decode.Decoder (Either ( ByteOffset, String ) ( ByteOffset, a ))


type alias ByteOffset =
    Int


runGetOrFail : Get a -> Bytes.Bytes -> Either ( ByteOffset, String ) a
runGetOrFail get bytes =
    case Bytes.Decode.decode (get (Bytes.width bytes)) bytes of
        Just (Right ( _, a )) ->
            Right a

        Just (Left ( remaining, err )) ->
            Left ( Bytes.width bytes - remaining, err )

        Nothing ->
            Left ( Bytes.width bytes, "binary encoding was corrupted" )



-- PRIMITIVES


fail : String -> Get a
fail err remaining =
    Bytes.Decode.succeed (Left ( remaining, err ))


checkedOffset : String -> ByteOffset -> ByteOffset -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (Either ( ByteOffset, String ) ( ByteOffset, a ))
checkedOffset context remaining needed decoder =
    if needed > remaining then
        Bytes.Decode.succeed (Left ( remaining, "not enough bytes for " ++ context ))

    else
        Bytes.Decode.map (\n -> Right ( remaining - needed, n )) decoder


getWord8 : Get Int
getWord8 remaining =
    checkedOffset "word8" remaining 1 Bytes.Decode.unsignedInt8


getWord16 : Get Int
getWord16 remaining =
    checkedOffset "word16" remaining 2 <| Bytes.Decode.unsignedInt16 Bytes.BE


getWord32 : Get Int
getWord32 remaining =
    checkedOffset "word32" remaining 4 <| Bytes.Decode.unsignedInt32 Bytes.BE


getString : Int -> Get String
getString width remaining =
    checkedOffset "string" remaining width (Bytes.Decode.string width)



-- INSTANCES


fmap : Functor.Fmap a (Get a) b (Get b)
fmap f g remaining =
    Bytes.Decode.map (Either.fmap (Tuple.mapSecond f)) (g remaining)


pure : Applicative.Pure a (Get a)
pure a remaining =
    Bytes.Decode.succeed (Right ( remaining, a ))


return : Monad.Return a (Get a)
return =
    pure


bind : Monad.Bind a (Get a) (Get b)
bind ga f remaining1 =
    Bytes.Decode.andThen
        (\ra ->
            case ra of
                Left ( remaining2, err ) ->
                    Bytes.Decode.succeed (Left ( remaining2, err ))

                Right ( remaining2, a ) ->
                    f a remaining2
        )
        (ga remaining1)



-- COMBINATORS


liftM2 : (a -> b -> c) -> Get a -> Get b -> Get c
liftM2 fun ga gb =
    bind ga <|
        \a ->
            bind gb <|
                \b ->
                    pure (fun a b)


liftM3 : (a -> b -> c -> d) -> Get a -> Get b -> Get c -> Get d
liftM3 fun ga gb gc =
    bind ga <|
        \a ->
            bind gb <|
                \b ->
                    bind gc <|
                        \c ->
                            pure (fun a b c)


liftM4 : (a -> b -> c -> d -> e) -> Get a -> Get b -> Get c -> Get d -> Get e
liftM4 fun ga gb gc gd =
    bind ga <|
        \a ->
            bind gb <|
                \b ->
                    bind gc <|
                        \c ->
                            bind gd <|
                                \d ->
                                    pure (fun a b c d)


liftM5 : (a -> b -> c -> d -> e -> f) -> Get a -> Get b -> Get c -> Get d -> Get e -> Get f
liftM5 fun ga gb gc gd ge =
    bind ga <|
        \a ->
            bind gb <|
                \b ->
                    bind gc <|
                        \c ->
                            bind gd <|
                                \d ->
                                    bind ge <|
                                        \e ->
                                            pure (fun a b c d e)


liftM6 : (a -> b -> c -> d -> e -> f -> g) -> Get a -> Get b -> Get c -> Get d -> Get e -> Get f -> Get g
liftM6 fun ga gb gc gd ge gf =
    bind ga <|
        \a ->
            bind gb <|
                \b ->
                    bind gc <|
                        \c ->
                            bind gd <|
                                \d ->
                                    bind ge <|
                                        \e ->
                                            bind gf <|
                                                \f ->
                                                    pure (fun a b c d e f)


getSequence : Get a -> Int -> Get (List a)
getSequence ga length remaining =
    Bytes.Decode.loop ( length, remaining, [] ) (getSequenceStep ga)


getSequenceStep : Get a -> ( Int, ByteOffset, List a ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, ByteOffset, List a ) (Either ( ByteOffset, String ) ( ByteOffset, List a )))
getSequenceStep ga ( length, remaining1, result ) =
    if length <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (Right ( remaining1, List.reverse result )))

    else
        Bytes.Decode.map
            (\ra ->
                case ra of
                    Left ( remaining2, err ) ->
                        Bytes.Decode.Done (Left ( remaining2, err ))

                    Right ( remaining2, a ) ->
                        Bytes.Decode.Loop ( length - 1, remaining2, a :: result )
            )
            (ga remaining1)
