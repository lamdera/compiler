module Test.Wire_Unconstructable exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


type Unconstructable
    = Unconstructable Unconstructable


expected_w3_encode_Unconstructable : Unconstructable -> Lamdera.Wire3.Encoder
expected_w3_encode_Unconstructable w3v =
    case w3v of
        Unconstructable v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 0
                , w3_encode_Unconstructable v0
                ]


expected_w3_decode_Unconstructable =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Unconstructable
                            |> Lamdera.Wire3.andMapDecode w3_decode_Unconstructable

                    _ ->
                        Lamdera.Wire3.failDecode
            )


type WithNever
    = WithNever Never


migrateNever : WithNever -> WithNever
migrateNever w3v =
    case w3v of
        WithNever v0 ->
            WithNever (never v0)
