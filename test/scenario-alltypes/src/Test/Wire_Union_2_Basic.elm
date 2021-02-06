module Test.Wire_Union_2_Basic exposing (..)

import Lamdera.Wire3


type Union2
    = Reversed
    | Middle
    | ShouldBeLast
    | Alphabetically


expected_w2_encode_Union2 w2v =
    case w2v of
        Alphabetically ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0 ]

        Middle ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1 ]

        Reversed ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 2 ]

        ShouldBeLast ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 3 ]


expected_w2_decode_Union2 =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Alphabetically

                    1 ->
                        Lamdera.Wire3.succeedDecode Middle

                    2 ->
                        Lamdera.Wire3.succeedDecode Reversed

                    3 ->
                        Lamdera.Wire3.succeedDecode ShouldBeLast

                    _ ->
                        Lamdera.Wire3.failDecode
            )
