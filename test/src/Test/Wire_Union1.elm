module Test.Wire_Union1 exposing (..)

import Lamdera.Wire2


type Union1
    = Hello
    | World


w2_encode_Union1 w2_e_val =
    case w2_e_val of
        Hello ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]

        World ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]


w2_decode_Union1 =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2_e_val ->
                case w2_e_val of
                    0 ->
                        Lamdera.Wire2.succeedDecode Hello

                    1 ->
                        Lamdera.Wire2.succeedDecode World

                    _ ->
                        Lamdera.Wire2.failDecode
            )
