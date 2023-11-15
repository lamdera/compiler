module Test.Wire_Record_Extensible5_ElmCss_External exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


type Compatible
    = Compatible


type alias LengthOrAutoOrCoverOrContain compatible =
    { compatible | value : String, lengthOrAutoOrCoverOrContain : Compatible }


expected_w3_encode_Compatible : Compatible -> Lamdera.Wire3.Encoder
expected_w3_encode_Compatible w3v =
    case w3v of
        Compatible ->
            Bytes.Encode.unsignedInt8
                0


expected_w3_decode_Compatible =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode
                            Compatible

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_LengthOrAutoOrCoverOrContain :
    ({ compatible | lengthOrAutoOrCoverOrContain : Compatible, value : String.String } -> Lamdera.Wire3.Encoder)
    -> LengthOrAutoOrCoverOrContain compatible
    -> Lamdera.Wire3.Encoder
expected_w3_encode_LengthOrAutoOrCoverOrContain w3_x_c_compatible =
    w3_x_c_compatible


expected_w3_decode_LengthOrAutoOrCoverOrContain w3_x_c_compatible =
    w3_x_c_compatible


type alias ColorValue compatible =
    { compatible | value : String, color : Compatible }


type alias Length compatible units =
    { compatible
        | value : String
        , length : Compatible
        , numericValue : Float
        , units : units
        , unitLabel : String
    }
