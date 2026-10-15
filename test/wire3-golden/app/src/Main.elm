port module Main exposing (main)

{-| Encodes fixed sample values of every golden type with the generated Wire3 codecs
and reports, through a port, each sample's bytes as hex plus two round-trip checks:
decode-then-encode gives the same bytes, and (for values without Bytes) the decoded
value equals the original. golden.js drives it and compares the hex with
expected/wire.txt, which was recorded with Lamdera 1.5.0 on Elm 0.19.2.
-}

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict
import Lamdera.Wire3 as W
import Other
import Platform
import SeqDict
import SeqSet
import Set
import Time
import Types exposing (..)


port go : (() -> msg) -> Sub msg


port out : List ( String, String, ( Bool, Bool ) ) -> Cmd msg


hex : Bytes -> String
hex bytes =
    let
        step ( n, acc ) =
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (String.concat (List.reverse acc)))

            else
                Bytes.Decode.unsignedInt8 |> Bytes.Decode.map (\b -> Bytes.Decode.Loop ( n - 1, hexByte b :: acc ))
    in
    Bytes.Decode.decode (Bytes.Decode.loop ( Bytes.width bytes, [] ) step) bytes
        |> Maybe.withDefault "<hex failed>"


hexByte : Int -> String
hexByte b =
    let
        digit d =
            String.slice d (d + 1) "0123456789abcdef"
    in
    digit (b // 16) ++ digit (modBy 16 b)


{-| One sample: its hex, whether decode-then-encode reproduces the bytes, and whether the
decoded value equals the original (skipped, reported as True, when compareValues is False:
Elm's == does not compare Bytes).
-}
sample : Bool -> String -> (a -> W.Encoder) -> W.Decoder a -> a -> ( String, String, ( Bool, Bool ) )
sample compareValues name enc dec value =
    let
        bytes =
            W.bytesEncode (enc value)

        decoded =
            W.bytesDecode dec bytes

        reencoded =
            decoded |> Maybe.map (\v -> hex (W.bytesEncode (enc v)))
    in
    ( name
    , hex bytes
    , ( reencoded == Just (hex bytes)
      , not compareValues || decoded == Just value
      )
    )


rec : Int -> Rec
rec i =
    { zeta = i * 7919 - 50000
    , a = "a-" ++ String.fromInt i ++ (if modBy 2 i == 0 then " ünï ✓ 😀" else "")
    , mid = toFloat i / 3.0
    , longerName = modBy 2 i == 0
    , b2 = Char.fromCode (65 + modBy 50 i)
    , ab = List.range 0 (modBy 4 i) |> List.map (\j -> j * 1000003 - 7)
    , zz = if modBy 3 i == 0 then Nothing else Just (-i)
    , aaa = ()
    , bA = if modBy 3 i == 0 then LT else if modBy 3 i == 1 then EQ else GT
    , b_ = i * i
    , b1 = String.repeat (modBy 3 i) "b1"
    }


ctors : List Ctors
ctors =
    [ Zz, A, Mid, LongerName, B2 -5, Ab "ab" 2.5, Aaa, AB, A_b, Ba Nothing, Ba (Just (Ba (Just AB))) ]


ctor : Int -> Ctors
ctor i =
    ctors |> List.drop (modBy (List.length ctors) i) |> List.head |> Maybe.withDefault Zz


nested : Int -> Nested
nested i =
    { outer = { zInner = i, aInner = { yy = "yy" ++ String.fromInt i, x = -i, xLonger = 0.125 * toFloat i } }
    , rec = rec i
    , list = List.range 0 (modBy 3 i) |> List.map (\j -> { q = j, pp = String.fromInt j, r = modBy 2 j == 1 })
    }


tree : Int -> a -> Tree a
tree d v =
    if d <= 0 then
        Leaf

    else
        Node (tree (d - 1) v) v (tree (d - 2) v)


forest : Int -> Forest
forest d =
    Forest
        { kids = if d == 0 then [] else List.map (\_ -> forest (d - 1)) (List.range 1 2)
        , nm = "f" ++ String.fromInt d
        , z = d * 3
        }


bytesOf : Int -> Bytes
bytesOf n =
    Bytes.Encode.encode (Bytes.Encode.sequence (List.map (\i -> Bytes.Encode.unsignedInt8 (modBy 256 (i * 37))) (List.range 0 n)))


containers : Int -> Containers
containers i =
    { zTuple = ( i, "t" ++ String.fromInt i )
    , triple = ( modBy 2 i == 0, -0.0 + toFloat i, ctor i )
    , maybe = if modBy 2 i == 0 then Just (rec i) else Nothing
    , result = if modBy 2 i == 0 then Ok (ctor (i + 1)) else Err "err"
    , resultOk = Ok (List.map ctor (List.range 0 i))
    , list = List.map rec (List.range 0 (modBy 3 i))
    , array = Array.fromList (List.map ctor (List.range i (i + 4)))
    , dict = Dict.fromList (List.map (\j -> ( "k" ++ String.fromInt j, rec j )) (List.range 0 (modBy 4 i)))
    , dictTuple = Dict.fromList [ ( ( 2, "b" ), 1.5 ), ( ( 1, "zz" ), -2 ), ( ( 1, "a" ), 1.5e300 ) ]
    , set = Set.fromList [ "zeta", "a", "mid", "", "ab" ]
    , seqDict = SeqDict.fromList (List.map (\j -> ( rec j, j )) (List.range 0 (modBy 3 i)))
    , seqDictCtor = SeqDict.fromList (List.map (\j -> ( ctor j, List.map ctor (List.range 0 j) )) (List.range 0 4))
    , seqSet = SeqSet.fromList (List.map ctor (List.range 0 (i + 6)))
    , seqSetRec = SeqSet.fromList (List.map rec (List.range 0 (modBy 2 i)))
    , bytes = bytesOf (i * 3)
    , time = Time.millisToPosix (1727600000000 + i)
    , unit = ()
    , order = GT
    }


person : Int -> Other.Person
person i =
    { zid = i, name = "p" ++ String.fromInt i, a = 1.0 / toFloat (i + 1), longest = String.repeat i "L", age = 40 + i, b = modBy 2 i == 0 }


otherCtor : Int -> Other.OtherCtor
otherCtor i =
    case modBy 5 i of
        0 ->
            Other.Zulu

        1 ->
            Other.Al i

        2 ->
            Other.MiddleOne "m" True

        3 ->
            Other.B

        _ ->
            Other.Bb [ Other.B, Other.Al -1, Other.Bb [] ]


usesExt : Int -> UsesExt
usesExt i =
    { p = person i
    , q = { extra = "x", ex = i, zid = -i, name = "q", a = 0.5 }
    , e = { k = i, kk = True, zz = [ 1, -2, 3 ], m = Just "m" }
    }


cross : Int -> Cross
cross i =
    { other =
        { zebra = otherCtor i
        , ab = Dict.fromList [ ( "b", 2 ), ( "a", 1 ) ]
        , abc = ( i, 0.1 )
        , a = if modBy 2 i == 0 then Just { yy = i, x = "x" } else Nothing
        }
    , ctor = otherCtor (i + 1)
    , opaque = Other.Opaque { inner = i, a = "opaque" }
    , wrapped = { value = if modBy 2 i == 0 then Zed (rec i) else Middle (rec (i + 1)) (ctor i), zcount = i, at = [ Al (ctor i), None_ ] }
    , tree = tree 3 (if modBy 2 i == 0 then Zed i else Al (String.fromInt i))
    }


frontendModel : Int -> FrontendModel
frontendModel i =
    { zeta = rec i, key = i, a = ctor i, nested = nested i }


backendModel : Int -> BackendModel
backendModel i =
    { containers = containers i
    , usesExt = usesExt i
    , cross = cross i
    , forest = forest (modBy 3 i)
    , trees = List.map (\j -> tree j (ctor j)) (List.range 0 3)
    , choice = if modBy 2 i == 0 then Middle i "s" else None_
    }


range : Int -> List Int
range n =
    List.range 0 (n - 1)


samples : List ( String, String, ( Bool, Bool ) )
samples =
    List.concat
        [ range 6 |> List.map (\i -> sample True ("Rec " ++ String.fromInt i) w3_encode_Rec w3_decode_Rec (rec i))
        , ctors |> List.indexedMap (\i c -> sample True ("Ctors " ++ String.fromInt i) w3_encode_Ctors w3_decode_Ctors c)
        , range 3 |> List.map (\i -> sample True ("Nested " ++ String.fromInt i) w3_encode_Nested w3_decode_Nested (nested i))
        , range 4 |> List.map (\i -> sample True ("Tree Int " ++ String.fromInt i) (w3_encode_Tree W.encodeInt) (w3_decode_Tree W.decodeInt) (tree i i))
        , [ Zed 1, Al "al", Middle -1 "mid", None_ ] |> List.indexedMap (\i c -> sample True ("Choice Int String " ++ String.fromInt i) (w3_encode_Choice W.encodeInt W.encodeString) (w3_decode_Choice W.decodeInt W.decodeString) c)
        , range 2 |> List.map (\i -> sample True ("Wrapped Rec " ++ String.fromInt i) (w3_encode_Wrapped w3_encode_Rec) (w3_decode_Wrapped w3_decode_Rec) { value = rec i, zcount = i, at = List.map rec (range i) })
        , range 3 |> List.map (\i -> sample True ("Forest " ++ String.fromInt i) w3_encode_Forest w3_decode_Forest (forest i))
        , range 3 |> List.map (\i -> sample False ("Containers " ++ String.fromInt i) w3_encode_Containers w3_decode_Containers (containers i))
        , range 3 |> List.map (\i -> sample True ("UsesExt " ++ String.fromInt i) w3_encode_UsesExt w3_decode_UsesExt (usesExt i))
        , range 5 |> List.map (\i -> sample True ("Cross " ++ String.fromInt i) w3_encode_Cross w3_decode_Cross (cross i))
        , range 5 |> List.map (\i -> sample True ("Other.OtherCtor " ++ String.fromInt i) Other.w3_encode_OtherCtor Other.w3_decode_OtherCtor (otherCtor i))
        , range 3 |> List.map (\i -> sample True ("Other.Person " ++ String.fromInt i) Other.w3_encode_Person Other.w3_decode_Person (person i))
        , range 3 |> List.map (\i -> sample True ("FrontendModel " ++ String.fromInt i) w3_encode_FrontendModel w3_decode_FrontendModel (frontendModel i))
        , range 3 |> List.map (\i -> sample False ("BackendModel " ++ String.fromInt i) w3_encode_BackendModel w3_decode_BackendModel (backendModel i))
        , [ FNoop, FzMsg (rec 1), FaMsg, F (Zed A), F (Al (nested 2)), F (Middle Zz (nested 0)), F None_ ] |> List.indexedMap (\i m -> sample True ("FrontendMsg " ++ String.fromInt i) w3_encode_FrontendMsg w3_decode_FrontendMsg m)
        , [ ToBackendZ AB, ToBackendA (Middle "s" 3), Tb, Tb2 (usesExt 1) ] |> List.indexedMap (\i m -> sample True ("ToBackend " ++ String.fromInt i) w3_encode_ToBackend w3_decode_ToBackend m)
        , [ BNoop, Bz -77, Bb_ (cross 2) ] |> List.indexedMap (\i m -> sample True ("BackendMsg " ++ String.fromInt i) w3_encode_BackendMsg w3_decode_BackendMsg m)
        , [ TfLonger (nested 1), Tf, T (containers 1) ] |> List.indexedMap (\i m -> sample False ("ToFrontend " ++ String.fromInt i) w3_encode_ToFrontend w3_decode_ToFrontend m)
        ]


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ m -> ( m, out samples )
        , subscriptions = \_ -> go (\_ -> ())
        }
