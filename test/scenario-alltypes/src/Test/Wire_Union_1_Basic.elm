module Test.Wire_Union_1_Basic exposing (..)

import Lamdera.Wire3


type Union1
    = Hello
    | World


expected_w3_encode_Union1 : Union1 -> Lamdera.Wire3.Encoder
expected_w3_encode_Union1 w3v =
    case w3v of
        Hello ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0 ]

        World ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1 ]


expected_w3_decode_Union1 =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Hello

                    1 ->
                        Lamdera.Wire3.succeedDecode World

                    _ ->
                        Lamdera.Wire3.failDecode
            )
