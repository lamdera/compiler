module Evergreen exposing (..)

import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


e_Char : Char -> E.Value
e_Char evg_p0 =
    E.int (Char.toCode evg_p0)


d_Char : D.Decoder Char
d_Char =
    D.int |> D.map Char.fromCode


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


union : String -> a -> D.Decoder a
union str final =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.succeed final
                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union1 : String -> D.Decoder a -> (a -> b) -> D.Decoder b
union1 str decoder constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.index 1 decoder |> D.map constructor
                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


atIndex idx decoder =
    custom (D.index idx decoder)



-- Trick from json-decode-pipeline


custom : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
custom =
    D.map2 (|>)
