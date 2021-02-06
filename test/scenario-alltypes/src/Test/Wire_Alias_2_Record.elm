module Test.Wire_Alias_2_Record exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire3
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
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeInt w2_rec_var0.int
            , Lamdera.Wire3.encodeFloat w2_rec_var0.float
            , Lamdera.Wire3.encodeBool w2_rec_var0.bool
            , Lamdera.Wire3.encodeChar w2_rec_var0.char
            , Lamdera.Wire3.encodeString w2_rec_var0.string
            , Lamdera.Wire3.encodeList Lamdera.Wire3.encodeInt w2_rec_var0.listInt
            , Lamdera.Wire3.encodeSet Lamdera.Wire3.encodeFloat w2_rec_var0.setFloat
            , Lamdera.Wire3.encodeArray Lamdera.Wire3.encodeString w2_rec_var0.arrayString
            , Lamdera.Wire3.encodeDict Lamdera.Wire3.encodeString (Lamdera.Wire3.encodeList Lamdera.Wire3.encodeInt) w2_rec_var0.dict
            , (\t -> Lamdera.Wire3.encodeInt (Time.posixToMillis t)) w2_rec_var0.time
            , Lamdera.Wire3.encodeOrder w2_rec_var0.order
            , Lamdera.Wire3.encodeUnit w2_rec_var0.unit
            ]


expected_w2_decode_AllTypes =
    Lamdera.Wire3.succeedDecode
        (\int0 float0 bool0 char0 string0 listInt0 setFloat0 arrayString0 dict0 time0 order0 unit0 ->
            { int = int0, float = float0, bool = bool0, char = char0, string = string0, listInt = listInt0, setFloat = setFloat0, arrayString = arrayString0, dict = dict0, time = time0, order = order0, unit = unit0 }
        )
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeChar
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeInt)
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeSet Lamdera.Wire3.decodeFloat)
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeArray Lamdera.Wire3.decodeString)
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeDict Lamdera.Wire3.decodeString (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeInt))
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeInt |> Lamdera.Wire3.andThenDecode (\t -> Lamdera.Wire3.succeedDecode (Time.millisToPosix t)))
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeOrder
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeUnit
