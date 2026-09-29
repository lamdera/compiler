port module Main exposing (..)

import Array
import Bytes exposing (Bytes)
import Dict
import Lamdera.Wire3 as W
import Platform
import Set
import Types exposing (..)


port out : Int -> Cmd msg


gen : Int -> Model
gen n =
    let
        rec i =
            { id = i * 7919 - 50000
            , score = toFloat i / 3.0
            , name = "name-" ++ String.fromInt i ++ (if modBy 5 i == 0 then " ünï ✓ 😀" else "")
            , active = modBy 2 i == 0
            , note = if modBy 3 i == 0 then Just ("note " ++ String.fromInt i) else Nothing
            , tags = List.map (\j -> "t" ++ String.fromInt j) (List.range 0 (modBy 4 i))
            , counts = List.range 0 (modBy 6 i) |> List.map (\j -> j * 1000003 - 7)
            }

        shape i =
            case modBy 4 i of
                0 -> Circle (toFloat i)
                1 -> Rect 1.5 (toFloat i)
                2 -> Empty
                _ -> Labelled ("s" ++ String.fromInt i) (if modBy 8 i == 3 then Just (Circle 2) else Nothing)

        tree d i =
            if d == 0 then Leaf else Node (tree (d - 1) (i * 2)) i (String.fromInt i) (tree (d - 1) (i * 2 + 1))
    in
    { recs = List.range 0 (n - 1) |> List.map (\i -> ( "key" ++ String.fromInt i, rec i )) |> Dict.fromList
    , ints = List.range 0 (n - 1) |> List.map (\i -> i * i * 31 - 1000 * i)
    , shapes = List.range 0 (n - 1) |> List.map shape
    , ids = List.range 0 (n // 2) |> List.map (\i -> "id" ++ String.fromInt i) |> Set.fromList
    , idList = List.range 0 (n // 4) |> List.map (\i -> Id ("x" ++ String.fromInt i))
    , colors = List.range 0 (n // 4) |> List.map (\i -> case modBy 3 i of
                                                           0 -> Red
                                                           1 -> Green
                                                           _ -> Blue)
    , tree = tree 8 1
    , db = { db = List.range 0 (n // 4) |> List.map (\i -> ( "k" ++ String.fromInt i, rec i )) |> Dict.fromList, seed = 42 }
    , arr = Array.fromList (List.range 0 (n - 1))
    , pair = ( -12345678901, "pair" )
    , res = if modBy 2 n == 0 then Ok 5 else Err "bad"
    , ch = '😀'
    , unit = ()
    }


edge : Int -> Model
edge k =
    let
        m =
            gen 5

        lone =
            String.fromChar (Char.fromCode 0xD800)

        loneLow =
            String.fromChar (Char.fromCode 0xDC00)

        weirdInts =
            [ 0, -1, 1, 107, 108, 215, 216, 4715, 4716, 9431, 9432, 32767, 32768, 65535, 65536, 8388607, 8388608, 16777216, 2147483647, 2147483648, 4294967295, 4294967296, 2 ^ 53, -(2 ^ 53), 2 ^ -1, 3 ^ -2, -(2 ^ -3), round (0 / 0), round (1 / 0), round (-1 / 0), 1000000000000 ]

        strs =
            [ "", "a", lone, loneLow, "x" ++ lone ++ "y", lone ++ loneLow, "\u{FEFF}bom at start of a long string ok", "long ascii string that is definitely longer than 24 bytes", "long unicode ünïcödé ✓✓✓ 😀😀 string over 24 bytes", "long with lone surrogate at end, over 24 bytes " ++ lone, String.repeat 5000 "é" ]
    in
    { m
        | ints = weirdInts |> List.drop k
        , recs =
            strs
                |> List.indexedMap (\i s -> ( s, { id = i, score = [ 0 / 0, 1 / 0, -1 / 0, -0.0, 1.5e300, 5.0e-324 ] |> List.drop (modBy 6 (i + k)) |> List.head |> Maybe.withDefault 0, name = s, active = True, note = Just s, tags = strs, counts = List.take 27 weirdInts ++ [ 1000000000000 ] } ))
                |> Dict.fromList
        , ch = if k < 30 then Char.fromCode (0xD800 + k) else Char.fromCode (0xDC00 + k)
    }


encode : Model -> Bytes
encode m =
    W.bytesEncode (w3_encode_Model m)


decode : Bytes -> Maybe Model
decode b =
    W.bytesDecode w3_decode_Model b


type alias Api =
    { gen : Int -> Model, edge : Int -> Model, encode : Model -> Bytes, decode : Bytes -> Maybe Model }


main : Program () Api ()
main =
    Platform.worker
        { init = \_ -> ( { gen = gen, edge = edge, encode = encode, decode = decode }, out 0 )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
