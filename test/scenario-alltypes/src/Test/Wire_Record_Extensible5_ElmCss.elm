module Test.Wire_Record_Extensible5_ElmCss exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import Test.Wire_Record_Extensible5_ElmCss_External


type alias LengthOrAutoOrCoverOrContain compatible =
    Test.Wire_Record_Extensible5_ElmCss_External.LengthOrAutoOrCoverOrContain compatible


expected_w3_encode_LengthOrAutoOrCoverOrContain :
    ({ compatible | lengthOrAutoOrCoverOrContain : Test.Wire_Record_Extensible5_ElmCss_External.Compatible, value : String.String } -> Lamdera.Wire3.Encoder)
    -> LengthOrAutoOrCoverOrContain compatible
    -> Lamdera.Wire3.Encoder
expected_w3_encode_LengthOrAutoOrCoverOrContain w3_x_c_compatible =
    Test.Wire_Record_Extensible5_ElmCss_External.w3_encode_LengthOrAutoOrCoverOrContain
        w3_x_c_compatible


type alias ColorStop compatibleA compatibleB unit =
    ( ColorValue compatibleA, Maybe (Length compatibleB unit) )


expected_w3_encode_ColorStop :
    ({ compatibleA
        | color : Test.Wire_Record_Extensible5_ElmCss_External.Compatible
        , value : String
     }
     -> Lamdera.Wire3.Encoder
    )
    ->
        ({ compatibleB
            | length : Test.Wire_Record_Extensible5_ElmCss_External.Compatible
            , numericValue : Float
            , unitLabel : String
            , units : unit
            , value : String
         }
         -> Lamdera.Wire3.Encoder
        )
    -> (unit -> Lamdera.Wire3.Encoder)
    -> (ColorStop compatibleA compatibleB unit -> Lamdera.Wire3.Encoder)
expected_w3_encode_ColorStop w3_x_c_compatibleA w3_x_c_compatibleB w3_x_c_unit =
    Lamdera.Wire3.encodePair
        (w3_encode_ColorValue
            w3_x_c_compatibleA
        )
        (Lamdera.Wire3.encodeMaybe
            (w3_encode_Length
                w3_x_c_compatibleB
                w3_x_c_unit
            )
        )


expected_w3_decode_ColorStop w3_x_c_compatibleA w3_x_c_compatibleB w3_x_c_unit =
    Lamdera.Wire3.decodePair
        (w3_decode_ColorValue
            w3_x_c_compatibleA
        )
        (Lamdera.Wire3.decodeMaybe
            (w3_decode_Length
                w3_x_c_compatibleB
                w3_x_c_unit
            )
        )


type alias ColorValue compatible =
    Test.Wire_Record_Extensible5_ElmCss_External.ColorValue compatible


type alias Length compatible units =
    Test.Wire_Record_Extensible5_ElmCss_External.Length compatible units
