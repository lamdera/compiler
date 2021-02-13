module Test.Wire_Core_Types exposing (..)

import Bytes
import Lamdera.Wire3
import Time


type CoreTypes
    = ValueTime Time.Posix
    | ValueBytes Bytes.Bytes


type alias Banned2ParamType msg =
    Platform.Router msg Int


expected_w3_encode_CoreTypes w3v =
    case w3v of
        ValueBytes v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, Lamdera.Wire3.encodeBytes v0 ]

        ValueTime v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1, (\t -> Lamdera.Wire3.encodeInt (Time.posixToMillis t)) v0 ]


expected_w3_decode_CoreTypes =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode ValueBytes |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBytes

                    1 ->
                        Lamdera.Wire3.succeedDecode ValueTime |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeInt |> Lamdera.Wire3.andThenDecode (\t -> Lamdera.Wire3.succeedDecode (Time.millisToPosix t)))

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_Banned2ParamType w3_x_c_msg =
    Lamdera.Wire3.failEncode


expected_w3_decode_Banned2ParamType w3_x_c_msg =
    Lamdera.Wire3.failDecode
