module Test.Wire_Alias_4_TvarRename exposing (..)

import Lamdera.Wire3
import Test.Wire_Alias_4_TvarRename2


{-| From terezka/elm-charts-alpha:ScatterChart.elm
-}
type alias Config data msg =
    { junk : Test.Wire_Alias_4_TvarRename2.Config data msg
    }


expected_w2_encode_Config w2_x_c_data w2_x_c_msg =
    \w2_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Test.Wire_Alias_4_TvarRename2.w2_encode_Config w2_x_c_data w2_x_c_msg w2_rec_var0.junk ]


expected_w2_decode_Config w2_x_c_data w2_x_c_msg =
    Lamdera.Wire3.succeedDecode (\junk0 -> { junk = junk0 }) |> Lamdera.Wire3.andMapDecode (Test.Wire_Alias_4_TvarRename2.w2_decode_Config w2_x_c_data w2_x_c_msg)


{-| From NoRedInk/style-elements
-}
type alias Grid msg =
    Test.Wire_Alias_4_TvarRename2.OnGrid (Maybe msg)


expected_w2_encode_Grid w2_x_c_msg =
    Test.Wire_Alias_4_TvarRename2.w2_encode_OnGrid (Lamdera.Wire3.encodeMaybe w2_x_c_msg)


expected_w2_decode_Grid w2_x_c_msg =
    Test.Wire_Alias_4_TvarRename2.w2_decode_OnGrid (Lamdera.Wire3.decodeMaybe w2_x_c_msg)


type Also msg
    = Tag (Test.Wire_Alias_4_TvarRename2.OnGrid (Maybe msg))


expected_w2_encode_Also w2_x_c_msg w2v =
    case w2v of
        Tag v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, Test.Wire_Alias_4_TvarRename2.w2_encode_OnGrid (Lamdera.Wire3.encodeMaybe w2_x_c_msg) v0 ]


expected_w2_decode_Also w2_x_c_msg =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Tag |> Lamdera.Wire3.andMapDecode (Test.Wire_Alias_4_TvarRename2.w2_decode_OnGrid (Lamdera.Wire3.decodeMaybe w2_x_c_msg))

                    _ ->
                        Lamdera.Wire3.failDecode
            )
