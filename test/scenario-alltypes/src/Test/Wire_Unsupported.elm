module Test.Wire_Unsupported exposing (..)

import Lamdera.Wire3


type alias RecordWithFunctions =
    { something : Int -> String }


expected_w3_encode_RecordWithFunctions : RecordWithFunctions -> Lamdera.Wire3.Encoder
expected_w3_encode_RecordWithFunctions =
    Lamdera.Wire3.failEncode


expected_w3_decode_RecordWithFunctions =
    Lamdera.Wire3.failDecode
