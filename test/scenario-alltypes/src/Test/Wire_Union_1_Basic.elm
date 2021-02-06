module Test.Wire_Union_1_Basic exposing (..)

import Lamdera.Wire3


type Union1
    = Hello
    | World


expected_w2_encode_Union1 w2v =
    case w2v of
        Hello ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0 ]

        World ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1 ]


expected_w2_decode_Union1 =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Hello

                    1 ->
                        Lamdera.Wire3.succeedDecode World

                    _ ->
                        Lamdera.Wire3.failDecode
            )
