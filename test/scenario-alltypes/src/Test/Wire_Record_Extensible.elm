module Test.Wire_Record_Extensible exposing (..)

import Lamdera.Wire3
import Test.Wire_Record_Extensible2 exposing (..)


type alias Config =
    { overlayColor : Color
    }


expected_w3_encode_Config : Config -> Lamdera.Wire3.Encoder
expected_w3_encode_Config =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Test.Wire_Record_Extensible2.w3_encode_Color w3_rec_var0.overlayColor ]


expected_w3_decode_Config =
    Lamdera.Wire3.succeedDecode (\overlayColor0 -> { overlayColor = overlayColor0 }) |> Lamdera.Wire3.andMapDecode Test.Wire_Record_Extensible2.w3_decode_Color



{- From datetimepicker-legacy/src/DateTimePicker/Config.elm -}
-- Extensible records are not supported at the moment, but this tests that we
-- handle the injection of failure encoders/decoders correctly


type Type msg
    = DateType (Config_ String msg)


type alias Config_ otherConfig msg =
    { otherConfig
        | attributes : List msg
    }


expected_w3_encode_Type : (msg -> Lamdera.Wire3.Encoder) -> Type msg -> Lamdera.Wire3.Encoder
expected_w3_encode_Type w3_x_c_msg w3v =
    case w3v of
        DateType v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w3_encode_Config_ Lamdera.Wire3.encodeString w3_x_c_msg v0 ]


expected_w3_decode_Type w3_x_c_msg =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode DateType |> Lamdera.Wire3.andMapDecode (w3_decode_Config_ Lamdera.Wire3.decodeString w3_x_c_msg)

                    _ ->
                        Lamdera.Wire3.failDecode
            )
