module Test.Wire_Record_Extensible1_Basic exposing (..)

import Bytes.Decode
import Lamdera.Wire3



-- From elm/css:Css.Internal.elm


type alias ColorValue compatible =
    { compatible | value : String }


{-| NOTE: this is generated, but it will never be called, because you can't have a `Named a` type
at runtime, it must always be concrete!
-}
expected_w3_encode_ColorValue : ({ compatible | value : String.String } -> Lamdera.Wire3.Encoder) -> ColorValue compatible -> Lamdera.Wire3.Encoder
expected_w3_encode_ColorValue w3_x_c_compatible =
    w3_x_c_compatible



-- Start off with the simplest extensible record scenario


type alias Color =
    ColorValue { red : Int, green : Int, blue : Int, alpha : Float }


expected_w3_encode_Color : Color -> Lamdera.Wire3.Encoder
expected_w3_encode_Color =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeFloat w3_rec_var0.alpha
            , Lamdera.Wire3.encodeInt w3_rec_var0.blue
            , Lamdera.Wire3.encodeInt w3_rec_var0.green
            , Lamdera.Wire3.encodeInt w3_rec_var0.red
            , Lamdera.Wire3.encodeString w3_rec_var0.value
            ]


expected_w3_decode_Color =
    Bytes.Decode.map5
        (\w3_v1 w3_v2 w3_v3 w3_v4 w3_v5 -> { alpha = w3_v1, blue = w3_v2, green = w3_v3, red = w3_v4, value = w3_v5 })
        Lamdera.Wire3.decodeFloat
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeString



-- Extend the simplest example with an overlapping field, the resulting wire functions should be identical


type alias ColorOverlap =
    ColorValue { value : String, red : Int, green : Int, blue : Int, alpha : Float }


expected_w3_encode_ColorOverlap : ColorOverlap -> Lamdera.Wire3.Encoder
expected_w3_encode_ColorOverlap =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeFloat w3_rec_var0.alpha
            , Lamdera.Wire3.encodeInt w3_rec_var0.blue
            , Lamdera.Wire3.encodeInt w3_rec_var0.green
            , Lamdera.Wire3.encodeInt w3_rec_var0.red
            , Lamdera.Wire3.encodeString w3_rec_var0.value
            ]


expected_w3_decode_ColorOverlap =
    Bytes.Decode.map5
        (\w3_v1 w3_v2 w3_v3 w3_v4 w3_v5 -> { alpha = w3_v1, blue = w3_v2, green = w3_v3, red = w3_v4, value = w3_v5 })
        Lamdera.Wire3.decodeFloat
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeInt
        Lamdera.Wire3.decodeString
