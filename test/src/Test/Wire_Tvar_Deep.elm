module Test.Wire_Tvar_Deep exposing (..)

import Lamdera.Wire2
import Test.Wire_Tvar_Deep2


type alias Specification ta =
    { modules : Test.Wire_Tvar_Deep2.Nested ta
    }


expected_w2_encode_Specification w2_x_c_ta =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Test.Wire_Tvar_Deep2.w2_encode_Nested w2_x_c_ta w2_rec_var0.modules ]


expected_w2_decode_Specification w2_x_c_ta =
    Lamdera.Wire2.succeedDecode (\modules0 -> { modules = modules0 }) |> Lamdera.Wire2.andMapDecode (Test.Wire_Tvar_Deep2.w2_decode_Nested w2_x_c_ta)
