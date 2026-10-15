port module Main exposing (..)

import Array
import Big
import Bytes exposing (Bytes)
import Bytes.Encode
import Dict
import Lamdera.Wire3 as W
import Platform
import Random
import SeqDict
import SeqSet
import Set
import Time
import Types exposing (..)
import Validated exposing (..)


port out : Int -> Cmd msg


rec : Int -> Rec
rec i =
    { id = i * 7919 - 50000
    , score = toFloat i / 3.0
    , name = "name-" ++ String.fromInt i ++ (if modBy 5 i == 0 then " ünï ✓ 😀 and some more text to pass eight bytes" else "")
    , active = modBy 2 i == 0
    , note = if modBy 3 i == 0 then Just ("note " ++ String.fromInt i) else Nothing
    , tags = List.map (\j -> "t" ++ String.fromInt j) (List.range 0 (modBy 4 i))
    , counts = List.range 0 (modBy 6 i) |> List.map (\j -> j * 1000003 - 7)
    }


shape : Int -> Shape
shape i =
    case modBy 4 i of
        0 -> Circle (toFloat i)
        1 -> Rect 1.5 (toFloat i)
        2 -> Empty
        _ -> Labelled ("s" ++ String.fromInt i) (if modBy 8 i == 3 then Just (Labelled "inner" (Just (Circle 2))) else Nothing)


color : Int -> Color
color i =
    case modBy 3 i of
        0 -> Red
        1 -> Green
        _ -> Blue


tree : Int -> Int -> Tree
tree d i =
    if d == 0 then Leaf else Node (tree (d - 1) (i * 2)) i (String.fromInt i) (tree (d - 1) (i * 2 + 1))


rose : Int -> Int -> Rose (Maybe Int)
rose d i =
    Rose (if modBy 2 i == 0 then Just i else Nothing) (if d == 0 then [] else List.map (rose (d - 1)) (List.range i (i + 2)))


forest : Int -> Forest
forest d =
    if d == 0 then NoForest else Forest (List.map (\i -> TreeB i (forest (d - 1))) (List.range 0 2))


prims : Int -> Prims
prims i =
    case modBy 8 i of
        0 -> PrimOrder (if modBy 3 i == 0 then LT else if modBy 3 i == 1 then EQ else GT)
        1 -> PrimBool (modBy 2 i == 0)
        2 -> PrimChar (Char.fromCode (65 + modBy 1000 i))
        3 -> PrimUnit ()
        4 -> PrimTriple ( i, "tri" ++ String.fromInt i, toFloat i * 0.25 )
        5 -> PrimResult (if modBy 2 i == 0 then Ok ("ok" ++ String.fromInt i) else Err i)
        6 -> PrimNever Nothing
        _ -> PrimNested [ Nothing, Just (Ok (Dict.fromList [ ( i, True ), ( -i, False ) ])), Just (Err "e") ]


bytesOf : Int -> Bytes
bytesOf n =
    Bytes.Encode.encode (Bytes.Encode.sequence (List.map (\i -> Bytes.Encode.unsignedInt8 (modBy 256 (i * 37))) (List.range 0 n)))


gen : Int -> Model
gen n =
    let
        range k = List.range 0 (k - 1)
    in
    { recs = range n |> List.map (\i -> ( "key" ++ String.fromInt i, rec i )) |> Dict.fromList
    , ints = range n |> List.map (\i -> i * i * 31 - 1000 * i)
    , floats = range (n // 4) |> List.map (\i -> toFloat i * 1.1 - 7)
    , shapes = range n |> List.map shape
    , ids = range (n // 2) |> List.map (\i -> "id" ++ String.fromInt i) |> Set.fromList
    , idList = range (n // 4) |> List.map (\i -> Id ("x" ++ String.fromInt i))
    , colors = range (n // 4) |> List.map color
    , tree = tree (min 10 (1 + n // 100)) 1
    , db = { db = range (n // 4) |> List.map (\i -> ( "k" ++ String.fromInt i, rec i )) |> Dict.fromList, seed = 42 }
    , dbNested = { db = range (n // 8) |> List.map (\i -> ( "n" ++ String.fromInt i, List.map (\j -> if modBy 2 j == 0 then Just (color j) else Nothing) (List.range 0 (modBy 5 i)) )) |> Dict.fromList, seed = -1 }
    , arr = Array.fromList (range n)
    , pair = ( -12345678901, "pair" )
    , res = if modBy 2 n == 0 then Ok 5 else Err "bad"
    , ch = '😀'
    , unit = ()
    , tagged = range (n // 8) |> List.map (\i -> if modBy 2 i == 0 then Tagged ("t" ++ String.fromInt i) { db = Dict.fromList [ ( "a", i ) ], seed = i } else Untagged { db = Dict.empty, seed = -i })
    , pair2 = Pair2 (Id "p") (Tagged "c" Blue) (range (n // 16) |> List.map (\i -> ( Id (String.fromInt i), Untagged (color i) )))
    , phantom = Phantom "ghost"
    , rose = rose (min 5 (1 + n // 200)) 0
    , forest = forest (min 5 (1 + n // 200))
    , person = { name = "Ann", age = 41 }
    , positioned = { label = "here", x = 1.5, y = -2.25 }
    , lazy = range (n // 16) |> List.map Evaluated
    , prims = range n |> List.map prims
    , time = Time.millisToPosix 1727600000000
    , times = range (n // 16) |> List.map (\i -> Time.millisToPosix (i * 86400000 - 5))
    , bytes = bytesOf (n * 3)
    , seed = Random.initialSeed (n * 17)
    , seqDict = range (n // 8) |> List.map (\i -> ( "z" ++ String.fromInt (n - i), List.range 0 (modBy 3 i) )) |> SeqDict.fromList
    , seqSet = range (n // 8) |> List.map (\i -> modBy 97 (i * 31)) |> SeqSet.fromList
    , big = range (n // 4) |> List.map Big.bigFrom
    , tupleKeys = range (n // 8) |> List.map (\i -> ( ( modBy 7 i, "k" ++ String.fromInt i ), toFloat i )) |> Dict.fromList
    , anon = { inner = { deep = range (modBy 9 n) }, flag = True }
    , holder = range (modBy 7 n) |> List.map (\i -> if i == 0 then HolderNone else Holder (modBy 2 i == 0) (Box (range i)) (if i > 2 then Just (Box (Just True)) else Nothing) ( { w = List.map (\j -> modBy 3 j == 0) (range i) }, Dict.fromList [ ( i, Just False ), ( -i, Nothing ) ] ))
    , prec = { zeta = 'z', alpha = Box (Dict.fromList [ ( "a", n ), ( "b", -n ) ]), mid = List.map (\i -> Char.fromCode (97 + modBy 26 i)) (range (modBy 11 n)), bB = Box ( 'q', if modBy 2 n == 0 then Just n else Nothing ) }
    , paged = { items = List.map String.fromInt (range (modBy 13 n)), pages = Box (range (modBy 5 n)) }
    , deepBox = Box (Box (List.map (\i -> Box (Dict.fromList [ ( String.fromInt i, range i ) ])) (range (modBy 6 n))))
    , nestedParam = Pair2 (Box (range 3)) { items = [ Box (Just 1), Box Nothing ], pages = Box [ n ] } [ ( Box [], { items = [], pages = Box [ 1, 2 ] } ) ]
    }


edge : Int -> Model
edge k =
    let
        m = gen 20
        lone = String.fromChar (Char.fromCode 0xD800)
        loneLow = String.fromChar (Char.fromCode 0xDC00)
        weirdInts =
            [ 0, -1, 1, 107, 108, 215, 216, 4715, 4716, 9431, 9432, 32767, 32768, 65535, 65536, 8388607, 8388608, 16777216, 2147483647, 2147483648, 4294967295, 4294967296, 2 ^ 53, -(2 ^ 53), 2 ^ -1, 3 ^ -2, -(2 ^ -3), round (0 / 0), round (1 / 0), round (-1 / 0), 1000000000000 ]
        strs =
            [ "", "a", lone, loneLow, "x" ++ lone ++ "y", lone ++ loneLow, "\u{FEFF}bom at start of a long string ok", "long ascii string that is definitely longer than 24 bytes", "long unicode ünïcödé ✓✓✓ 😀😀 string over 24 bytes", "long with lone surrogate at end, over 24 bytes " ++ lone, String.repeat 5000 "é", "\u{FFFD} literal replacement char, long enough", "12345678", "123456789", "ünï" ]
    in
    { m
        | ints = weirdInts |> List.drop k
        , floats = [ 0 / 0, 1 / 0, -1 / 0, -0.0, 1.5e300, 5.0e-324 ]
        , recs =
            strs
                |> List.indexedMap (\i s -> ( s, { id = i, score = [ 0 / 0, 1 / 0, -1 / 0, -0.0, 1.5e300, 5.0e-324 ] |> List.drop (modBy 6 (i + k)) |> List.head |> Maybe.withDefault 0, name = s, active = True, note = Just s, tags = strs, counts = List.take 27 weirdInts ++ [ 1000000000000 ] } ))
                |> Dict.fromList
        , ch = if k < 30 then Char.fromCode (0xD800 + k) else Char.fromCode (0xDC00 + k)
        , lazy = if k == 1 then [ Lazy (\_ -> 1), Evaluated 2 ] else [ Evaluated 3 ]
        , bytes = bytesOf 0
    }


-- VALIDATED TYPES (Validated.elm). `vgen n` makes a VModel that passes every validator,
-- except when n is a multiple of 4: then one or two of the thirteen rejection sources
-- below (`bad k`) are switched on, at various nesting depths, so the oracles see both
-- outcomes and every kind of rejection.


vtree : (Int -> Bool) -> Int -> Int -> VTree
vtree bad d i =
    if d == 0 then
        VLeaf (if bad 1 && modBy 3 i == 0 then -5 else modBy 1000 i)

    else
        VNode (vtree bad (d - 1) (i * 2)) (if bad 2 && d == 1 then 13 else if i == 13 then 14 else i) (vtree bad (d - 1) (i * 2 + 1))


vtreeBigLeaf : Int -> VTree
vtreeBigLeaf i =
    VNode (VNode (VLeaf 5000) 2 (VLeaf 1)) i (VLeaf 0)


vrose : (Int -> Bool) -> Int -> Int -> VRose Int
vrose bad d i =
    VRose i (if d == 0 then [] else List.map (vrose bad (d - 1)) (List.range i (i + modBy 4 (i + d) - 1 + (if bad 4 && d == 1 then 4 else 0))))


vping : (Int -> Bool) -> Int -> Int -> VPing
vping bad d i =
    VPing (if i == 7 then 8 else i)
        (if d == 0 then Nothing else Just (VPong (if bad 8 && d == 1 then "bad" else "p" ++ String.fromInt i) (List.map (vping bad (d - 1)) (List.range 0 (modBy 3 i - 1)))))


vrec : (Int -> Bool) -> Int -> Int -> VRec
vrec bad d i =
    if d == 0 then
        VRecNone

    else
        VRec { name = String.repeat (1 + modBy 8 i + (if bad 10 && d == 1 then 8 else 0)) "n", scores = Dict.fromList (List.map (\j -> ( String.fromInt j, Positive j )) (List.range 0 (modBy 4 i))), child = Just (vrec bad (d - 1) (i + 1)) }


vgen : Int -> VModel
vgen n =
    let
        range k = List.range 0 (k - 1)
        m = modBy 97 n
        pick = modBy 13 (n // 4)
        bad k = modBy 4 n == 0 && (pick == k || (modBy 8 n == 0 && modBy 13 (pick + 5) == k))
    in
    { positives = range (modBy 9 n) |> List.map (\i -> Positive (if bad 0 && i == modBy 9 n - 1 then -1 else i * n))
    , tree =
        if bad 3 then
            VNode (vtree bad 2 n) 1 (vtreeBigLeaf n)

        else
            vtree bad (min 6 (1 + modBy 6 n)) n
    , roses = range (1 + modBy 4 n) |> List.map (\i -> ( "r" ++ String.fromInt i, vrose bad (1 + modBy 3 (n + i)) (i + n) )) |> Dict.fromList
    , rosePair = VRose ( Positive m, "x" ) [ VRose ( Positive (m + 1), "y" ) [] ]
    , pair = if modBy 2 n == 0 && not (bad 5) then Nothing else Just (VPair "a" (Positive m) (Just (VPair "b" (Positive (if bad 5 then -m - 1 else m)) (Just VPairEnd))))
    , enums = range (1 + modBy 5 n) |> List.map (\i -> if bad 6 && i == modBy 5 n then VC else if modBy 2 i == 0 then VA else VB)
    , box = VBox (range (modBy 5 n) |> List.map (\i -> Positive (if bad 7 then 400 else modBy 50 (i * m))))
    , ping =
        if bad 12 then
            VPing 1 (Just (VPong "q" [ vping bad 1 n, VPing 7 Nothing ]))

        else
            vping bad (1 + modBy 3 n) n
    , phantom = VPhantom (if bad 9 then 99 else if n == 99 then 100 else n)
    , rec = vrec bad (1 + modBy 3 n) n
    , unvalidated = Unvalidated (range (modBy 3 n) |> List.map (\i -> Positive (if bad 11 then i - 1 else i))) (Just (if modBy 2 n == 0 then VA else VB))
    , keyed = range (modBy 4 n) |> List.map (\i -> ( i * 3 - 4, [ Nothing, Just (vtree bad (modBy 3 i) (n + i)) ] )) |> Dict.fromList
    }


vencode : VModel -> Bytes
vencode m =
    W.bytesEncode (w3_encode_VModel m)


vdecode : Bytes -> Maybe VModel
vdecode b =
    W.bytesDecode w3_decode_VModel b


vdecodeUnsafe : Bytes -> Maybe VModel
vdecodeUnsafe b =
    W.bytesDecode w3_unsafe_decode_VModel b


vdecoder : W.Decoder VModel
vdecoder =
    w3_decode_VModel


vdecoderUnsafe : W.Decoder VModel
vdecoderUnsafe =
    w3_unsafe_decode_VModel


decodeVTree : Bytes -> Maybe VTree
decodeVTree b =
    W.bytesDecode w3_decode_VTree b


decodeVTreeUnsafe : Bytes -> Maybe VTree
decodeVTreeUnsafe b =
    W.bytesDecode w3_unsafe_decode_VTree b


encode : Model -> Bytes
encode m =
    W.bytesEncode (w3_encode_Model m)


decode : Bytes -> Maybe Model
decode b =
    W.bytesDecode w3_decode_Model b


encodeHasFn : HasFn -> Bytes
encodeHasFn h =
    W.bytesEncode (w3_encode_HasFn h)


decodeHasFn : Bytes -> Maybe HasFn
decodeHasFn b =
    W.bytesDecode w3_decode_HasFn b


hasFns : List HasFn
hasFns =
    [ HasFn { f = \x -> x, n = 3 }, NoFn 7 ]


decodeFat3 : Bytes -> Maybe Fat3
decodeFat3 b =
    W.bytesDecode w3_decode_Fat3 b


decodeFat : Bytes -> Maybe Fat
decodeFat b =
    W.bytesDecode w3_decode_Fat b


decodeTree : Bytes -> Maybe Tree
decodeTree b =
    W.bytesDecode w3_decode_Tree b


decodeBig : Bytes -> Maybe Big.Big
decodeBig b =
    W.bytesDecode Big.w3_decode_Big b


decodeSNaN : Bytes -> Maybe SNaN
decodeSNaN b =
    W.bytesDecode w3_decode_SNaN b


encodeSNaN : SNaN -> Bytes
encodeSNaN v =
    W.bytesEncode (w3_encode_SNaN v)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


depthProbes : List ( String, Bytes -> Bool )
depthProbes =
    [ ( "Fat3", decodeFat3 >> isJust ), ( "Fat", decodeFat >> isJust ), ( "Tree", decodeTree >> isJust ), ( "Big", decodeBig >> isJust ), ( "VTree", decodeVTree >> isJust ), ( "VTreeUnsafe", decodeVTreeUnsafe >> isJust ) ]


type alias Api =
    { gen : Int -> Model
    , edge : Int -> Model
    , encode : Model -> Bytes
    , decode : Bytes -> Maybe Model
    , encodeHasFn : HasFn -> Bytes
    , decodeHasFn : Bytes -> Maybe HasFn
    , hasFns : List HasFn
    , depth : List ( String, Bytes -> Bool )
    , decodeSNaN : Bytes -> Maybe SNaN
    , encodeSNaN : SNaN -> Bytes
    , vgen : Int -> VModel
    , vencode : VModel -> Bytes
    , vdecode : Bytes -> Maybe VModel
    , vdecodeUnsafe : Bytes -> Maybe VModel
    , vdecoder : W.Decoder VModel
    , vdecoderUnsafe : W.Decoder VModel
    }


main : Program () Api ()
main =
    Platform.worker
        { init = \_ -> ( { gen = gen, edge = edge, encode = encode, decode = decode, encodeHasFn = encodeHasFn, decodeHasFn = decodeHasFn, hasFns = hasFns, depth = depthProbes, decodeSNaN = decodeSNaN, encodeSNaN = encodeSNaN, vgen = vgen, vencode = vencode, vdecode = vdecode, vdecodeUnsafe = vdecodeUnsafe, vdecoder = vdecoder, vdecoderUnsafe = vdecoderUnsafe }, out 0 )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
