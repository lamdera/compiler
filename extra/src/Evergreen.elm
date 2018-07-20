module Evergreen exposing (..)

import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


-- eqPosStr : Int -> String -> a -> D.Decoder a
-- eqPosStr idx str final =
--     D.index idx D.string
--         |> D.andThen
--             (\s ->
--                 if s == str then
--                     D.succeed final
--                 else
--                     D.fail "wire decode failed for Msg"
--             )
--
--
-- eqPosStr1 : Int -> String -> D.Decoder a -> (a -> b) -> D.Decoder b
-- eqPosStr1 idx str subDecoder constructor =
--     D.index idx D.string
--         |> D.andThen
--             (\s ->
--                 if s == str then
--                     subDecoder |> D.andThen (\v1 -> D.succeed <| constructor v1)
--                 else
--                     D.fail "wire decode failed for Msg"
--             )
--
--
-- objStringAt : List String -> D.Decoder a -> D.Decoder a
-- objStringAt fields decoder =
--     D.at fields D.string
--         |> D.map (D.decodeString decoder)
--         |> D.andThen
--             (\r ->
--                 case r of
--                     Err x ->
--                         D.fail x
--
--                     Ok x ->
--                         D.succeed x
--             )
--
--
-- decodeAny : String -> (String -> D.Decoder a) -> a -> a
-- decodeAny string chainDecoder default =
--     D.decodeString
--         (D.field "version" D.string
--             |> D.andThen chainDecoder
--         )
--         string
--         -- |> Debug.log "decodeAny"
--         |> Result.withDefault default
--
--
-- decodeAnyM : String -> (String -> D.Decoder a) -> a -> a
-- decodeAnyM string chainDecoderM default =
--     D.decodeString
--         (D.index 0 D.string
--             |> D.andThen chainDecoderM
--         )
--         string
--         |> Result.withDefault default
--
--
-- Move elsewhere later


e_Char : Char -> E.Value
e_Char evg_p0 =
    E.int (Char.toCode evg_p0)


d_Char : D.Decoder Char
d_Char =
    D.int |> D.map (\evg_v0 -> Char.fromCode evg_v0)


e_Order : Order -> E.Value
e_Order evg_p0 =
    case evg_p0 of
        LT ->
            E.string "LT"

        EQ ->
            E.string "EQ"

        GT ->
            E.string "GT"


d_Order : D.Decoder Order
d_Order =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "LT" ->
                        D.succeed LT

                    "EQ" ->
                        D.succeed EQ

                    "GT" ->
                        D.succeed GT

                    _ ->
                        D.fail <| "unexpected Order value: " ++ s
            )


d_set : D.Decoder comparable -> D.Decoder (Set comparable)
d_set decoder =
    D.list decoder |> D.map Set.fromList


eqPosStr : Int -> String -> a -> D.Decoder a
eqPosStr idx str final =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.succeed final
                else
                    D.fail "evergreen wire decode failed for union type"
            )


eqPosStr1 : Int -> String -> D.Decoder a -> (a -> b) -> D.Decoder b
eqPosStr1 idx str subDecoder constructor =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    subDecoder |> D.andThen (\v1 -> D.succeed <| constructor v1)
                else
                    D.fail "evergreen wire decode failed for union type"
            )


atIndex idx decoder =
    custom (D.index idx decoder)



-- Trick from json-decode-pipeline


custom : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
custom =
    D.map2 (|>)
