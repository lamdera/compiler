module Test.Wire_Alias_2_Record exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
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
    }


expected_w2_encode_AllTypes =
    \w2_rec_var0 ->
        Lamdera.Wire2.encodeSequenceWithoutLength
            [ Lamdera.Wire2.encodeInt w2_rec_var0.int
            , Lamdera.Wire2.encodeFloat w2_rec_var0.float
            , Lamdera.Wire2.encodeBool w2_rec_var0.bool
            , Lamdera.Wire2.encodeChar w2_rec_var0.char
            , Lamdera.Wire2.encodeString w2_rec_var0.string
            , Lamdera.Wire2.encodeList Lamdera.Wire2.encodeInt w2_rec_var0.listInt
            , Lamdera.Wire2.encodeSet Lamdera.Wire2.encodeFloat w2_rec_var0.setFloat
            , Lamdera.Wire2.encodeArray Lamdera.Wire2.encodeString w2_rec_var0.arrayString
            , Lamdera.Wire2.encodeDict Lamdera.Wire2.encodeString (Lamdera.Wire2.encodeList Lamdera.Wire2.encodeInt) w2_rec_var0.dict
            , (\t -> Lamdera.Wire2.encodeInt (Time.posixToMillis t)) w2_rec_var0.time
            , Lamdera.Wire2.encodeOrder w2_rec_var0.order
            , Lamdera.Wire2.encodeUnit w2_rec_var0.unit
            ]


expected_w2_decode_AllTypes =
    Lamdera.Wire2.succeedDecode
        (\int0 float0 bool0 char0 string0 listInt0 setFloat0 arrayString0 dict0 time0 order0 unit0 ->
            { int = int0, float = float0, bool = bool0, char = char0, string = string0, listInt = listInt0, setFloat = setFloat0, arrayString = arrayString0, dict = dict0, time = time0, order = order0, unit = unit0 }
        )
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeFloat
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBool
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeChar
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeString
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeInt)
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeSet Lamdera.Wire2.decodeFloat)
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeArray Lamdera.Wire2.decodeString)
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeDict Lamdera.Wire2.decodeString (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeInt))
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeInt |> Lamdera.Wire2.andThenDecode (\t -> Lamdera.Wire2.succeedDecode (Time.millisToPosix t)))
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeOrder
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeUnit
