module Test.Wire_Union_2_Basic exposing (..)

import Lamdera.Wire2


type Union2
    = Reversed
    | Middle
    | ShouldBeLast
    | Alphabetically


expected_w2_encode_Union2 w2v =
    case w2v of
        Alphabetically ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]

        Middle ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]

        Reversed ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2 ]

        ShouldBeLast ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 3 ]


expected_w2_decode_Union2 =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Alphabetically

                    1 ->
                        Lamdera.Wire2.succeedDecode Middle

                    2 ->
                        Lamdera.Wire2.succeedDecode Reversed

                    3 ->
                        Lamdera.Wire2.succeedDecode ShouldBeLast

                    _ ->
                        Lamdera.Wire2.failDecode
            )
