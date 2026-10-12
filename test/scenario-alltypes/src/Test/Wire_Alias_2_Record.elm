module Test.Wire_Alias_2_Record exposing (..)

import Array exposing (Array)
import Bytes.Decode
import Dict exposing (Dict)
import Lamdera.Wire3
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Test.External exposing (..)
import Time


type alias AllTypes =
    { int : Int
    , float : Float
    , bool : Bool
    , char : Char
    , string : String
    , listInt : List Int
    , setFloat : Set Float
    , arrayString : Array String
    , dict : Dict String (List Int)
    , time : Time.Posix
    , order : Order

    -- , union : AllUnion
    , unit : ()

    -- , externalCustomBasic : ExternalCustomBasic
    -- , externalCustom : ExternalCustom Int
    , seqDict : SeqDict String Int
    , seqSet : SeqSet Int
    }


expected_w3_encode_AllTypes : AllTypes -> Lamdera.Wire3.Encoder
expected_w3_encode_AllTypes =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeArray Lamdera.Wire3.encodeString w3_rec_var0.arrayString
            , Lamdera.Wire3.encodeBool w3_rec_var0.bool
            , Lamdera.Wire3.encodeChar w3_rec_var0.char
            , Lamdera.Wire3.encodeDict Lamdera.Wire3.encodeString (Lamdera.Wire3.encodeList Lamdera.Wire3.encodeInt) w3_rec_var0.dict
            , Lamdera.Wire3.encodeFloat w3_rec_var0.float
            , Lamdera.Wire3.encodeInt w3_rec_var0.int
            , Lamdera.Wire3.encodeList Lamdera.Wire3.encodeInt w3_rec_var0.listInt
            , Lamdera.Wire3.encodeOrder w3_rec_var0.order
            , Lamdera.Wire3.encodeSet Lamdera.Wire3.encodeFloat w3_rec_var0.setFloat
            , Lamdera.Wire3.encodeString w3_rec_var0.string
            , (\t -> Lamdera.Wire3.encodeInt (Time.posixToMillis t)) w3_rec_var0.time
            , Lamdera.Wire3.encodeUnit w3_rec_var0.unit
            ]


expected_w3_decode_AllTypes =
    Bytes.Decode.map3
        (\w3_v1 w3_v2 w3_v3 -> { arrayString = w3_v1.w3_1, bool = w3_v1.w3_2, char = w3_v1.w3_3, dict = w3_v1.w3_4, float = w3_v1.w3_5, int = w3_v2.w3_1, listInt = w3_v2.w3_2, order = w3_v2.w3_3, seqDict = w3_v2.w3_4, seqSet = w3_v2.w3_5, setFloat = w3_v3.w3_1, string = w3_v3.w3_2, time = w3_v3.w3_3, unit = w3_v3.w3_4 })
        (Bytes.Decode.map5
            (\w3_v1 w3_v2 w3_v3 w3_v4 w3_v5 -> { w3_1 = w3_v1, w3_2 = w3_v2, w3_3 = w3_v3, w3_4 = w3_v4, w3_5 = w3_v5 })
            (Lamdera.Wire3.decodeArray Lamdera.Wire3.decodeString)
            Lamdera.Wire3.decodeBool
            Lamdera.Wire3.decodeChar
            (Lamdera.Wire3.decodeDict Lamdera.Wire3.decodeString (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeInt))
            Lamdera.Wire3.decodeFloat
        )
        (Bytes.Decode.map5
            (\w3_v1 w3_v2 w3_v3 w3_v4 w3_v5 -> { w3_1 = w3_v1, w3_2 = w3_v2, w3_3 = w3_v3, w3_4 = w3_v4, w3_5 = w3_v5 })
            Lamdera.Wire3.decodeInt
            (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeInt)
            Lamdera.Wire3.decodeOrder
            (SeqDict.decodeDict Lamdera.Wire3.decodeString Lamdera.Wire3.decodeInt)
            (SeqSet.decodeSet Lamdera.Wire3.decodeInt)
        )
        (Bytes.Decode.map4
            (\w3_v1 w3_v2 w3_v3 w3_v4 -> { w3_1 = w3_v1, w3_2 = w3_v2, w3_3 = w3_v3, w3_4 = w3_v4 })
            (Lamdera.Wire3.decodeSet Lamdera.Wire3.decodeFloat)
            Lamdera.Wire3.decodeString
            (Lamdera.Wire3.decodeInt |> Lamdera.Wire3.andThenDecode (\t -> Lamdera.Wire3.succeedDecode (Time.millisToPosix t)))
            Lamdera.Wire3.decodeUnit
        )
