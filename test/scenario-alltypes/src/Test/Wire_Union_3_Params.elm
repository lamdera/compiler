module Test.Wire_Union_3_Params exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire3
import Set exposing (Set)


type UnionParams
    = ValueStandalone
    | ValueInt Int
    | ValueFloat Float
    | ValueBool Bool
    | ValueChar Char
    | ValueString String
    | ValueOrder Order
    | ValueUnit ()
    | ValueTwoParams Bool Char
    | ValueMaybe (Maybe String)
    | ValueListBool (List Bool)
    | ValueSetFloat (Set Float)
    | ValueArrayString (Array String)
    | ValueResult (Result String Int)
    | ValueDict (Dict String (List Int))
    | ValueTuple ( Int, String )
    | ValueTriple ( Int, String, Bool )
    | ValueRecord { field1 : Int, field2 : String }


expected_w2_encode_UnionParams w2v =
    case w2v of
        ValueArrayString v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, Lamdera.Wire3.encodeArray Lamdera.Wire3.encodeString v0 ]

        ValueBool v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1, Lamdera.Wire3.encodeBool v0 ]

        ValueChar v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 2, Lamdera.Wire3.encodeChar v0 ]

        ValueDict v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 3, Lamdera.Wire3.encodeDict Lamdera.Wire3.encodeString (Lamdera.Wire3.encodeList Lamdera.Wire3.encodeInt) v0 ]

        ValueFloat v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 4, Lamdera.Wire3.encodeFloat v0 ]

        ValueInt v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 5, Lamdera.Wire3.encodeInt v0 ]

        ValueListBool v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 6, Lamdera.Wire3.encodeList Lamdera.Wire3.encodeBool v0 ]

        ValueMaybe v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 7, Lamdera.Wire3.encodeMaybe Lamdera.Wire3.encodeString v0 ]

        ValueOrder v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 8, Lamdera.Wire3.encodeOrder v0 ]

        ValueRecord v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 9, (\w2_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeInt w2_rec_var0.field1, Lamdera.Wire3.encodeString w2_rec_var0.field2 ]) v0 ]

        ValueResult v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 10, Lamdera.Wire3.encodeResult Lamdera.Wire3.encodeString Lamdera.Wire3.encodeInt v0 ]

        ValueSetFloat v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 11, Lamdera.Wire3.encodeSet Lamdera.Wire3.encodeFloat v0 ]

        ValueStandalone ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 12 ]

        ValueString v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 13, Lamdera.Wire3.encodeString v0 ]

        ValueTriple v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 14, Lamdera.Wire3.encodeTriple Lamdera.Wire3.encodeInt Lamdera.Wire3.encodeString Lamdera.Wire3.encodeBool v0 ]

        ValueTuple v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 15, Lamdera.Wire3.encodePair Lamdera.Wire3.encodeInt Lamdera.Wire3.encodeString v0 ]

        ValueTwoParams v0 v1 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 16, Lamdera.Wire3.encodeBool v0, Lamdera.Wire3.encodeChar v1 ]

        ValueUnit v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 17, Lamdera.Wire3.encodeUnit v0 ]


expected_w2_decode_UnionParams =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode ValueArrayString |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeArray Lamdera.Wire3.decodeString)

                    1 ->
                        Lamdera.Wire3.succeedDecode ValueBool |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool

                    2 ->
                        Lamdera.Wire3.succeedDecode ValueChar |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeChar

                    3 ->
                        Lamdera.Wire3.succeedDecode ValueDict |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeDict Lamdera.Wire3.decodeString (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeInt))

                    4 ->
                        Lamdera.Wire3.succeedDecode ValueFloat |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat

                    5 ->
                        Lamdera.Wire3.succeedDecode ValueInt |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt

                    6 ->
                        Lamdera.Wire3.succeedDecode ValueListBool |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeBool)

                    7 ->
                        Lamdera.Wire3.succeedDecode ValueMaybe |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeMaybe Lamdera.Wire3.decodeString)

                    8 ->
                        Lamdera.Wire3.succeedDecode ValueOrder |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeOrder

                    9 ->
                        Lamdera.Wire3.succeedDecode ValueRecord |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.succeedDecode (\field10 field20 -> { field1 = field10, field2 = field20 }) |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString)

                    10 ->
                        Lamdera.Wire3.succeedDecode ValueResult |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeResult Lamdera.Wire3.decodeString Lamdera.Wire3.decodeInt)

                    11 ->
                        Lamdera.Wire3.succeedDecode ValueSetFloat |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeSet Lamdera.Wire3.decodeFloat)

                    12 ->
                        Lamdera.Wire3.succeedDecode ValueStandalone

                    13 ->
                        Lamdera.Wire3.succeedDecode ValueString |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString

                    14 ->
                        Lamdera.Wire3.succeedDecode ValueTriple |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeTriple Lamdera.Wire3.decodeInt Lamdera.Wire3.decodeString Lamdera.Wire3.decodeBool)

                    15 ->
                        Lamdera.Wire3.succeedDecode ValueTuple |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodePair Lamdera.Wire3.decodeInt Lamdera.Wire3.decodeString)

                    16 ->
                        Lamdera.Wire3.succeedDecode ValueTwoParams |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeChar

                    17 ->
                        Lamdera.Wire3.succeedDecode ValueUnit |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeUnit

                    _ ->
                        Lamdera.Wire3.failDecode
            )
