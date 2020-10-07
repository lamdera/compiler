module Test.Wire_Union_3_Params exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
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


expected_w2_encode_UnionParams w2v =
    case w2v of
        ValueArrayString v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, Lamdera.Wire2.encodeArray Lamdera.Wire2.encodeString v0 ]

        ValueBool v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, Lamdera.Wire2.encodeBool v0 ]

        ValueChar v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2, Lamdera.Wire2.encodeChar v0 ]

        ValueDict v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 3, Lamdera.Wire2.encodeDict Lamdera.Wire2.encodeString (Lamdera.Wire2.encodeList Lamdera.Wire2.encodeInt) v0 ]

        ValueFloat v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 4, Lamdera.Wire2.encodeFloat v0 ]

        ValueInt v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 5, Lamdera.Wire2.encodeInt v0 ]

        ValueListBool v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 6, Lamdera.Wire2.encodeList Lamdera.Wire2.encodeBool v0 ]

        ValueMaybe v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 7, Lamdera.Wire2.encodeMaybe Lamdera.Wire2.encodeString v0 ]

        ValueOrder v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 8, Lamdera.Wire2.encodeOrder v0 ]

        ValueResult v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 9, Lamdera.Wire2.encodeResult Lamdera.Wire2.encodeString Lamdera.Wire2.encodeInt v0 ]

        ValueSetFloat v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 10, Lamdera.Wire2.encodeSet Lamdera.Wire2.encodeFloat v0 ]

        ValueStandalone ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 11 ]

        ValueString v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 12, Lamdera.Wire2.encodeString v0 ]

        ValueTriple v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 13, Lamdera.Wire2.encodeTriple Lamdera.Wire2.encodeInt Lamdera.Wire2.encodeString Lamdera.Wire2.encodeBool v0 ]

        ValueTuple v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 14, Lamdera.Wire2.encodePair Lamdera.Wire2.encodeInt Lamdera.Wire2.encodeString v0 ]

        ValueTwoParams v0 v1 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 15, Lamdera.Wire2.encodeBool v0, Lamdera.Wire2.encodeChar v1 ]

        ValueUnit v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 16, Lamdera.Wire2.encodeUnit v0 ]


expected_w2_decode_UnionParams =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueArrayString |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeArray Lamdera.Wire2.decodeString)

                    1 ->
                        Lamdera.Wire2.succeedDecode ValueBool |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBool

                    2 ->
                        Lamdera.Wire2.succeedDecode ValueChar |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeChar

                    3 ->
                        Lamdera.Wire2.succeedDecode ValueDict |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeDict Lamdera.Wire2.decodeString (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeInt))

                    4 ->
                        Lamdera.Wire2.succeedDecode ValueFloat |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeFloat

                    5 ->
                        Lamdera.Wire2.succeedDecode ValueInt |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt

                    6 ->
                        Lamdera.Wire2.succeedDecode ValueListBool |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeBool)

                    7 ->
                        Lamdera.Wire2.succeedDecode ValueMaybe |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeMaybe Lamdera.Wire2.decodeString)

                    8 ->
                        Lamdera.Wire2.succeedDecode ValueOrder |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeOrder

                    9 ->
                        Lamdera.Wire2.succeedDecode ValueResult |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeResult Lamdera.Wire2.decodeString Lamdera.Wire2.decodeInt)

                    10 ->
                        Lamdera.Wire2.succeedDecode ValueSetFloat |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeSet Lamdera.Wire2.decodeFloat)

                    11 ->
                        Lamdera.Wire2.succeedDecode ValueStandalone

                    12 ->
                        Lamdera.Wire2.succeedDecode ValueString |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeString

                    13 ->
                        Lamdera.Wire2.succeedDecode ValueTriple |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeTriple Lamdera.Wire2.decodeInt Lamdera.Wire2.decodeString Lamdera.Wire2.decodeBool)

                    14 ->
                        Lamdera.Wire2.succeedDecode ValueTuple |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodePair Lamdera.Wire2.decodeInt Lamdera.Wire2.decodeString)

                    15 ->
                        Lamdera.Wire2.succeedDecode ValueTwoParams |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBool |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeChar

                    16 ->
                        Lamdera.Wire2.succeedDecode ValueUnit |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeUnit

                    _ ->
                        Lamdera.Wire2.failDecode
            )
