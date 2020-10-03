module Test.Wire_Union2 exposing (..)

import Lamdera.Wire2



-- @TODO add more items here so we're longer and more complex than Union1


type Union2
    = Reversed
    | Middle
    | Alphabetically


w2_encode_Union2 w2_e_val =
    case w2_e_val of
        Alphabetically ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]

        Middle ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]

        Reversed ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2 ]


w2_decode_Union2 =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2_e_val ->
                case w2_e_val of
                    0 ->
                        Lamdera.Wire2.succeedDecode Alphabetically

                    1 ->
                        Lamdera.Wire2.succeedDecode Middle

                    2 ->
                        Lamdera.Wire2.succeedDecode Reversed

                    _ ->
                        Lamdera.Wire2.failDecode
            )
