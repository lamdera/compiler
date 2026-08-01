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
                        Lamdera.Wire3.succeedDecode WrapsBasicRecord |> Lamdera.Wire3.andMapDecode Test.External.w3_decode_ExternalRecordBasic

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
                        -- ExternalRecordViaExtensible is an extensible-record alias chain, so the
                        -- decoder reifies it inline instead of calling the foreign codec. This body
                        -- matches expected_w3_decode_ExternalRecordViaExtensible in Test.External —
                        -- same fields, same order, same bytes. The encoder still calls the codec.
                        Lamdera.Wire3.succeedDecode WrapsExtensibleRecord
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode (\base0 green0 red0 -> { base = base0, green = green0, red = red0 })
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_WrapsInRecord : WrapsInRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_WrapsInRecord w3v =
    case w3v of
        WrapsInRecord v0 ->
            -- The payload is an anonymous record, so it is encoded AS a record: a nested
            -- encodeSequenceWithoutLength over its fields, rather than reaching straight for
            -- .field. Byte-identical, since encodeSequenceWithoutLength writes no length
            -- prefix and this record has exactly one field.
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 0
                , (\w3_rec_var0 ->
                    Lamdera.Wire3.encodeSequenceWithoutLength
                        [ Test.External.w3_encode_ExternalRecordViaExtensible w3_rec_var0.field ]
                  )
                    v0
                ]


expected_w3_decode_WrapsInRecord =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode WrapsInRecord
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode (\field0 -> { field = field0 })
                                    -- Reified inline for the same reason as WrapsExtensibleRecord above.
                                    |> Lamdera.Wire3.andMapDecode
                                        (Lamdera.Wire3.succeedDecode (\base0 green0 red0 -> { base = base0, green = green0, red = red0 })
                                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                        )
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )
