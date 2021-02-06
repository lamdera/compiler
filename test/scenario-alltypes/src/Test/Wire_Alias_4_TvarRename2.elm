module Test.Wire_Alias_4_TvarRename2 exposing (..)

import Lamdera.Wire3
import Test.Wire_Alias_4_TvarRename3


type alias Config element msg =
    ( element, msg )


expected_w2_encode_Config w2_x_c_element w2_x_c_msg =
    Lamdera.Wire3.encodePair w2_x_c_element w2_x_c_msg


expected_w2_decode_Config w2_x_c_element w2_x_c_msg =
    Lamdera.Wire3.decodePair w2_x_c_element w2_x_c_msg


type alias OnGrid thing =
    Test.Wire_Alias_4_TvarRename3.OnGrid thing
