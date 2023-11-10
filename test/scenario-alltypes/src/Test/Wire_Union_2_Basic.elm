module Test.Wire_Union_2_Basic exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


type Union2
    = Reversed
    | Middle
    | ShouldBeLast
    | Alphabetically


expected_w3_encode_Union2 : Union2 -> Lamdera.Wire3.Encoder
expected_w3_encode_Union2 w3v =
    case w3v of
        Alphabetically ->
            Bytes.Encode.unsignedInt8 0

        Middle ->
            Bytes.Encode.unsignedInt8 1

        Reversed ->
            Bytes.Encode.unsignedInt8 2

        ShouldBeLast ->
            Bytes.Encode.unsignedInt8 3


expected_w3_decode_Union2 =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
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
