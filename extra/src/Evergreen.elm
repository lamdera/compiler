module Evergreen exposing (atIndex, custom, d_char, d_dict, d_order, d_set, d_time, d_tuple, d_unit, e_char, e_dict, e_order, e_time, e_tuple, e_unit, union, union1)

import Char
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Time


e_char : Char -> E.Value
e_char evg_p0 =
    E.int (Char.toCode evg_p0)


d_char : D.Decoder Char
d_char =
    D.int |> D.map Char.fromCode


e_order : Order -> E.Value
e_order evg_p0 =
    case evg_p0 of
        LT ->
            E.string "LT"

        EQ ->
            E.string "EQ"

        GT ->
            E.string "GT"


d_order : D.Decoder Order
d_order =
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


e_time : Time.Posix -> E.Value
e_time t =
    E.int <| Time.posixToMillis t


d_time : D.Decoder Time.Posix
d_time =
    D.int |> D.map (\t -> Time.millisToPosix t)


e_dict : (comparable -> E.Value) -> (v -> E.Value) -> Dict comparable v -> E.Value
e_dict k_encode v_encode dict =
    dict
        |> Dict.toList
        |> List.map (\t -> e_tuple k_encode v_encode t)
        |> E.list identity


d_dict : D.Decoder comparable -> D.Decoder v -> D.Decoder (Dict comparable v)
d_dict k_decode v_decode =
    D.list (d_tuple k_decode v_decode)
        |> D.map Dict.fromList


e_tuple : (a -> E.Value) -> (b -> E.Value) -> ( a, b ) -> E.Value
e_tuple a_encode b_encode ( a, b ) =
    E.list identity [ a_encode a, b_encode b ]


d_tuple : D.Decoder a -> D.Decoder b -> D.Decoder ( a, b )
d_tuple a b =
    D.index 0 a
        |> D.andThen
            (\aVal ->
                D.index 1 b
                    |> D.andThen (\bVal -> D.succeed ( aVal, bVal ))
            )


e_unit : E.Value
e_unit =
    E.null


d_unit : D.Decoder ()
d_unit =
    D.null ()


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
