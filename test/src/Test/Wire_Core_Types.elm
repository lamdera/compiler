module Test.Wire_Core_Types exposing (..)

import Bytes
import Lamdera.Wire2
import Time


type CoreTypes
    = ValueTime Time.Posix
    | ValueBytes Bytes.Bytes


expected_w2_encode_CoreTypes w2v =
    case w2v of
        ValueBytes v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, Lamdera.Wire2.encodeBytes v0 ]

        ValueTime v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, Time.w2_encode_Posix v0 ]


expected_w2_decode_CoreTypes =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueBytes |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBytes

                    1 ->
                        Lamdera.Wire2.succeedDecode ValueTime |> Lamdera.Wire2.andMapDecode Time.w2_decode_Posix

                    _ ->
                        Lamdera.Wire2.failDecode
            )
