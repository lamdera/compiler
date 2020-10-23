module Test.Wire_Record_Extensible exposing (..)

import Lamdera.Wire2
import Test.Wire_Record_Extensible2 exposing (..)



-- Extensible records are not supported at the moment, but this tests that we
-- handle the injection of failure encoders/decoders correctly


type alias Config =
    { overlayColor : Color
    }



{- This fails on comparison as our type generation is more specific than type inference gives, but the
   gen itself type checks fine.
-}
--
--
--
--
-- expected_w2_encode_Config =
--     \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Test.Wire_Record_Extensible2.w2_encode_Color w2_rec_var0.overlayColor ]
--
--
-- expected_w2_decode_Config =
--     Lamdera.Wire2.succeedDecode (\overlayColor0 -> { overlayColor = overlayColor0 }) |> Lamdera.Wire2.andMapDecode Test.Wire_Record_Extensible2.w2_decode_Color
--
--
--
{- From datetimepicker-legacy/src/DateTimePicker/Config.elm

   Tests neutered as our generation types are more specific than old Source based ones,
   but the actual test was for a generation failure so this ensures the gen at least type checks

-}


type Type msg
    = DateType (Config_ String msg)


type alias Config_ otherConfig msg =
    { otherConfig
        | attributes : List msg
    }


expected_w2_encode_Type w2_x_c_msg w2v =
    case w2v of
        DateType v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_encode_Config_ Lamdera.Wire2.encodeString w2_x_c_msg v0 ]


expected_w2_decode_Type w2_x_c_msg =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode DateType |> Lamdera.Wire2.andMapDecode (w2_decode_Config_ Lamdera.Wire2.decodeString w2_x_c_msg)

                    _ ->
                        Lamdera.Wire2.failDecode
            )
