module Test.Wire_Validate_Union exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


type ValidatedUnion
    = OptionA
    | OptionB Int


w3_validate_ValidatedUnion : ValidatedUnion -> Result String ()
w3_validate_ValidatedUnion val =
    case val of
        OptionB n ->
            if n < 0 then
                Err "OptionB value must be non-negative"

            else
                Ok ()

        _ ->
            Ok ()


expected_w3_encode_ValidatedUnion : ValidatedUnion -> Lamdera.Wire3.Encoder
expected_w3_encode_ValidatedUnion w3v =
    case w3v of
        OptionA ->
            Bytes.Encode.unsignedInt8 0

        OptionB v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 1, Lamdera.Wire3.encodeInt v0 ]
