module Test.Wire_Alias_4_TvarRename2 exposing (..)

import Lamdera.Wire3
import Test.Wire_Alias_4_TvarRename3


type alias Config element msg =
    ( element, msg )


expected_w3_encode_Config : (element -> Lamdera.Wire3.Encoder) -> (msg -> Lamdera.Wire3.Encoder) -> Config element msg -> Lamdera.Wire3.Encoder
expected_w3_encode_Config w3_x_c_element w3_x_c_msg =
    Lamdera.Wire3.encodePair w3_x_c_element w3_x_c_msg


expected_w3_decode_Config w3_x_c_element w3_x_c_msg =
    Lamdera.Wire3.decodePair w3_x_c_element w3_x_c_msg


type alias OnGrid thing =
    Test.Wire_Alias_4_TvarRename3.OnGrid thing
