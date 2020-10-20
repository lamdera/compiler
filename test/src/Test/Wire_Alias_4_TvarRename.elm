module Test.Wire_Alias_4_TvarRename exposing (..)

import Lamdera.Wire2
import Test.Wire_Alias_4_TvarRename2



-- From terezka/elm-charts-alpha:ScatterChart.elm


type alias Config data msg =
    { junk : Test.Wire_Alias_4_TvarRename2.Config data msg
    }



-- This test actually passes, but fails the diff because we generate Tvars that are more specific in name (i.e. data, msg, element) than type inference adds (i.e. a, b, c)
-- Check it manually for big changes.
--
-- expected_w2_encode_Config w2_x_c_data w2_x_c_msg =
--     \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Test.Wire_Alias_4_TvarRename2.w2_encode_Config w2_x_c_data w2_x_c_msg w2_rec_var0.junk ]
--
--
-- expected_w2_decode_Config w2_x_c_data w2_x_c_msg =
--     Lamdera.Wire2.succeedDecode (\junk0 -> { junk = junk0 }) |> Lamdera.Wire2.andMapDecode (Test.Wire_Alias_4_TvarRename2.w2_decode_Config w2_x_c_data w2_x_c_msg)
