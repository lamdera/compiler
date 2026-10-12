module Test.Wire_Tvar_Deep exposing (..)

import Bytes.Decode
import Lamdera.Wire3
import Test.Wire_Tvar_Deep2


type alias Specification ta =
    { modules : Test.Wire_Tvar_Deep2.Nested ta
    }


expected_w3_encode_Specification : (ta -> Lamdera.Wire3.Encoder) -> Specification ta -> Lamdera.Wire3.Encoder
expected_w3_encode_Specification w3_x_c_ta =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Test.Wire_Tvar_Deep2.w3_encode_Nested w3_x_c_ta w3_rec_var0.modules ]


expected_w3_decode_Specification w3_x_c_ta =
    Bytes.Decode.map
        (\w3_v1 -> { modules = w3_v1 })
        (Test.Wire_Tvar_Deep2.w3_decode_Nested w3_x_c_ta)
