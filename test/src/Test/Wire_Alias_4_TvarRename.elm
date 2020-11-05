module Test.Wire_Alias_4_TvarRename exposing (..)

import Lamdera.Wire2
import Test.Wire_Alias_4_TvarRename2


{-| From terezka/elm-charts-alpha:ScatterChart.elm
-}
type alias Config data msg =
    { junk : Test.Wire_Alias_4_TvarRename2.Config data msg
    }


expected_w2_encode_Config w2_x_c_data w2_x_c_msg =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Test.Wire_Alias_4_TvarRename2.w2_encode_Config w2_x_c_data w2_x_c_msg w2_rec_var0.junk ]


expected_w2_decode_Config w2_x_c_data w2_x_c_msg =
    Lamdera.Wire2.succeedDecode (\junk0 -> { junk = junk0 }) |> Lamdera.Wire2.andMapDecode (Test.Wire_Alias_4_TvarRename2.w2_decode_Config w2_x_c_data w2_x_c_msg)


{-| From NoRedInk/style-elements
-}
type alias Grid msg =
    Test.Wire_Alias_4_TvarRename2.OnGrid (Maybe msg)


expected_w2_encode_Grid w2_x_c_msg =
    Test.Wire_Alias_4_TvarRename2.w2_encode_OnGrid (Lamdera.Wire2.encodeMaybe w2_x_c_msg)


expected_w2_decode_Grid w2_x_c_msg =
    Test.Wire_Alias_4_TvarRename2.w2_decode_OnGrid (Lamdera.Wire2.decodeMaybe w2_x_c_msg)


type Also msg
    = Tag (Test.Wire_Alias_4_TvarRename2.OnGrid (Maybe msg))


expected_w2_encode_Also w2_x_c_msg w2v =
    case w2v of
        Tag v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, Test.Wire_Alias_4_TvarRename2.w2_encode_OnGrid (Lamdera.Wire2.encodeMaybe w2_x_c_msg) v0 ]


expected_w2_decode_Also w2_x_c_msg =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Tag |> Lamdera.Wire2.andMapDecode (Test.Wire_Alias_4_TvarRename2.w2_decode_OnGrid (Lamdera.Wire2.decodeMaybe w2_x_c_msg))

                    _ ->
                        Lamdera.Wire2.failDecode
            )
