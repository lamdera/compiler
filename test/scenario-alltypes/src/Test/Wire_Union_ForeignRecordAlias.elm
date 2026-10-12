module Test.Wire_Union_ForeignRecordAlias exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import Test.External exposing (ExternalRecordBasic, ExternalRecordViaExtensible)


{-| Regression test: extensible record aliases through alias chains.
See: <https://github.com/elm-explorations/test/pull/249#issuecomment-4076937757>
-}
type WrapsBasicRecord
    = WrapsBasicRecord ExternalRecordBasic


type WrapsExtensibleRecord
    = WrapsExtensibleRecord ExternalRecordViaExtensible


type WrapsInRecord
    = WrapsInRecord { field : ExternalRecordViaExtensible }


expected_w3_encode_WrapsBasicRecord : WrapsBasicRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_WrapsBasicRecord w3v =
    case w3v of
        WrapsBasicRecord v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, Test.External.w3_encode_ExternalRecordBasic v0 ]


expected_w3_decode_WrapsBasicRecord =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Bytes.Decode.map
                            (\w3_v1 -> WrapsBasicRecord w3_v1)
                            Test.External.w3_decode_ExternalRecordBasic

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_WrapsExtensibleRecord : WrapsExtensibleRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_WrapsExtensibleRecord w3v =
    case w3v of
        WrapsExtensibleRecord v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, Test.External.w3_encode_ExternalRecordViaExtensible v0 ]


expected_w3_decode_WrapsExtensibleRecord =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Bytes.Decode.map
                            (\w3_v1 -> WrapsExtensibleRecord w3_v1)
                            (Bytes.Decode.map3
                                (\w3_v1 w3_v2 w3_v3 -> { base = w3_v1, green = w3_v2, red = w3_v3 })
                                Lamdera.Wire3.decodeString
                                Lamdera.Wire3.decodeInt
                                Lamdera.Wire3.decodeInt
                            )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_WrapsInRecord : WrapsInRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_WrapsInRecord w3v =
    case w3v of
        WrapsInRecord v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, Test.External.w3_encode_ExternalRecordViaExtensible v0.field ]


expected_w3_decode_WrapsInRecord =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Bytes.Decode.map
                            (\w3_v1 -> WrapsInRecord w3_v1)
                            (Bytes.Decode.map
                                (\w3_v1 -> { field = w3_v1 })
                                (Bytes.Decode.map3
                                    (\w3_v1 w3_v2 w3_v3 -> { base = w3_v1, green = w3_v2, red = w3_v3 })
                                    Lamdera.Wire3.decodeString
                                    Lamdera.Wire3.decodeInt
                                    Lamdera.Wire3.decodeInt
                                )
                            )

                    _ ->
                        Lamdera.Wire3.failDecode
            )
