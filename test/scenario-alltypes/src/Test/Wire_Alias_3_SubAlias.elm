module Test.Wire_Alias_3_SubAlias exposing (..)

import Array exposing (Array)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Lamdera.Wire3
import Set exposing (Set)


type alias AliasThreaded threadedTvar =
    SubRecordAlias threadedTvar


type alias SubRecordAlias threadedTvar =
    { tvar : threadedTvar }


type alias RecordThreadedWrap threadedTvar =
    { tvar : BannedTypeDeepAlias threadedTvar }


type alias BannedTypeDeepAlias msg =
    Dict.Dict Float (List (Int -> msg))


type alias RecordParams msg =
    { subCustom : List (SubCustom msg)
    }


type SubCustom value
    = SubCustomValue value


expected_w3_encode_AliasThreaded : (threadedTvar -> Lamdera.Wire3.Encoder) -> AliasThreaded threadedTvar -> Lamdera.Wire3.Encoder
expected_w3_encode_AliasThreaded w3_x_c_threadedTvar =
    w3_encode_SubRecordAlias w3_x_c_threadedTvar


expected_w3_decode_AliasThreaded w3_x_c_threadedTvar =
    w3_decode_SubRecordAlias w3_x_c_threadedTvar


expected_w3_encode_SubRecordAlias : (threadedTvar -> Lamdera.Wire3.Encoder) -> SubRecordAlias threadedTvar -> Lamdera.Wire3.Encoder
expected_w3_encode_SubRecordAlias w3_x_c_threadedTvar =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ w3_x_c_threadedTvar w3_rec_var0.tvar ]


expected_w3_decode_SubRecordAlias w3_x_c_threadedTvar =
    Lamdera.Wire3.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire3.andMapDecode w3_x_c_threadedTvar


expected_w3_encode_RecordThreadedWrap : (threadedTvar -> Lamdera.Wire3.Encoder) -> RecordThreadedWrap threadedTvar -> Lamdera.Wire3.Encoder
expected_w3_encode_RecordThreadedWrap w3_x_c_threadedTvar =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ w3_encode_BannedTypeDeepAlias w3_x_c_threadedTvar w3_rec_var0.tvar
            ]


expected_w3_decode_RecordThreadedWrap w3_x_c_threadedTvar =
    Lamdera.Wire3.succeedDecode (\tvar0 -> { tvar = tvar0 })
        |> Lamdera.Wire3.andMapDecode (w3_decode_BannedTypeDeepAlias w3_x_c_threadedTvar)


expected_w3_encode_BannedTypeDeepAlias : (msg -> Lamdera.Wire3.Encoder) -> BannedTypeDeepAlias msg -> Lamdera.Wire3.Encoder
expected_w3_encode_BannedTypeDeepAlias w3_x_c_msg =
    Lamdera.Wire3.encodeDict Lamdera.Wire3.encodeFloat (Lamdera.Wire3.encodeList Lamdera.Wire3.failEncode)


expected_w3_decode_BannedTypeDeepAlias w3_x_c_msg =
    Lamdera.Wire3.decodeDict Lamdera.Wire3.decodeFloat (Lamdera.Wire3.decodeList Lamdera.Wire3.failDecode)


expected_w3_encode_RecordParams : (msg -> Lamdera.Wire3.Encoder) -> RecordParams msg -> Lamdera.Wire3.Encoder
expected_w3_encode_RecordParams w3_x_c_msg =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeList (w3_encode_SubCustom w3_x_c_msg) w3_rec_var0.subCustom ]


expected_w3_decode_RecordParams w3_x_c_msg =
    Lamdera.Wire3.succeedDecode (\subCustom0 -> { subCustom = subCustom0 }) |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList (w3_decode_SubCustom w3_x_c_msg))


expected_w3_encode_SubCustom : (value -> Lamdera.Wire3.Encoder) -> SubCustom value -> Lamdera.Wire3.Encoder
expected_w3_encode_SubCustom w3_x_c_value w3v =
    case w3v of
        SubCustomValue v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, w3_x_c_value v0 ]


expected_w3_decode_SubCustom w3_x_c_value =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode SubCustomValue |> Lamdera.Wire3.andMapDecode w3_x_c_value

                    _ ->
                        Lamdera.Wire3.failDecode
            )
