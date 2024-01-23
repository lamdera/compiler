module Test.Wire_Record_Extensible2_MultiParam exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


{-| Because extensible records are always re-ified at runtime usage with concrete values,
these type encoders/decoders will never get called.

@TODO can we just remove them entirely?

-}
type alias ExtensibleRecordType comparable a =
    { a | id : comparable }


expected_w3_encode_ExtensibleRecordType : (comparable -> Lamdera.Wire3.Encoder) -> ({ a | id : comparable } -> Lamdera.Wire3.Encoder) -> ExtensibleRecordType comparable a -> Lamdera.Wire3.Encoder
expected_w3_encode_ExtensibleRecordType w3_x_c_comparable w3_x_c_a =
    w3_x_c_a


expected_w3_decode_ExtensibleRecordType w3_x_c_comparable w3_x_c_a =
    w3_x_c_a



-- Try taking a concrete record alias and passing it as the extensible record tvar


type alias RecordType =
    { atLeastOneField : String
    }


type alias ExtensibleRecordTypeUsage =
    ExtensibleRecordType Int RecordType


expected_w3_encode_ExtensibleRecordTypeUsage : ExtensibleRecordTypeUsage -> Lamdera.Wire3.Encoder
expected_w3_encode_ExtensibleRecordTypeUsage =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeString w3_rec_var0.atLeastOneField
            , Lamdera.Wire3.encodeInt w3_rec_var0.id
            ]


expected_w3_decode_ExtensibleRecordTypeUsage =
    Lamdera.Wire3.succeedDecode
        (\atLeastOneField0 id0 -> { atLeastOneField = atLeastOneField0, id = id0 })
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt



-- Try taking an anonymous record and passing it as the extensible record tvar


type alias ExtensibleRecordTypeUsage_ =
    ExtensibleRecordType Int { name : String }


expected_w3_encode_ExtensibleRecordTypeUsage_ : ExtensibleRecordTypeUsage_ -> Lamdera.Wire3.Encoder
expected_w3_encode_ExtensibleRecordTypeUsage_ =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeInt w3_rec_var0.id
            , Lamdera.Wire3.encodeString w3_rec_var0.name
            ]


expected_w3_decode_ExtensibleRecordTypeUsage_ =
    Lamdera.Wire3.succeedDecode
        (\id0 name0 -> { id = id0, name = name0 })
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString



-- Try reference a sub-aliased concrete record and expect the encoder/decoder to delegate as per normal


type C_
    = NodeClicked_ ExtensibleRecordTypeUsage_


expected_w3_encode_C_ : C_ -> Lamdera.Wire3.Encoder
expected_w3_encode_C_ w3v =
    case w3v of
        NodeClicked_ v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 0
                , w3_encode_ExtensibleRecordTypeUsage_ v0
                ]


expected_w3_decode_C_ =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode NodeClicked_ |> Lamdera.Wire3.andMapDecode w3_decode_ExtensibleRecordTypeUsage_

                    _ ->
                        Lamdera.Wire3.failDecode
            )
