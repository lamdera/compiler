module Test.Wire_Union_1_Basic exposing (..)

import Lamdera.Wire2


type Union1
    = Hello
    | World


expected_w2_encode_Union1 w2v =
    case w2v of
        Hello ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]

        World ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]


expected_w2_decode_Union1 =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Hello

                    1 ->
                        Lamdera.Wire2.succeedDecode World

                    _ ->
                        Lamdera.Wire2.failDecode
            )
