module Evergreen exposing (..)

import Json.Decode as D


eqPosStr : Int -> String -> a -> D.Decoder a
eqPosStr idx str final =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.succeed final
                else
                    D.fail "wire decode failed for Msg"
            )


eqPosStr1 : Int -> String -> D.Decoder a -> (a -> b) -> D.Decoder b
eqPosStr1 idx str subDecoder constructor =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    subDecoder |> D.andThen (\v1 -> D.succeed <| constructor v1)
                else
                    D.fail "wire decode failed for Msg"
            )


objStringAt : List String -> D.Decoder a -> D.Decoder a
objStringAt fields decoder =
    D.at fields D.string
        |> D.map (D.decodeString decoder)
        |> D.andThen
            (\r ->
                case r of
                    Err x ->
                        D.fail x

                    Ok x ->
                        D.succeed x
            )


decodeAny : String -> (String -> D.Decoder a) -> a -> a
decodeAny string chainDecoder default =
    D.decodeString
        (D.field "version" D.string
            |> D.andThen chainDecoder
        )
        string
        -- |> Debug.log "decodeAny"
        |> Result.withDefault default


decodeAnyM : String -> (String -> D.Decoder a) -> a -> a
decodeAnyM string chainDecoderM default =
    D.decodeString
        (D.index 0 D.string
            |> D.andThen chainDecoderM
        )
        string
        |> Result.withDefault default
