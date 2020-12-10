module Test.Wire_Alias_3_SubAlias exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
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


expected_w2_encode_AliasThreaded w2_x_c_threadedTvar =
    w2_encode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_decode_AliasThreaded w2_x_c_threadedTvar =
    w2_decode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_encode_SubRecordAlias w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_SubRecordAlias w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar


expected_w2_encode_RecordThreadedWrap w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ w2_encode_BannedTypeDeepAlias w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_RecordThreadedWrap w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode (w2_decode_BannedTypeDeepAlias w2_x_c_threadedTvar)


expected_w2_encode_BannedTypeDeepAlias w2_x_c_msg =
    Lamdera.Wire2.encodeDict Lamdera.Wire2.encodeFloat (Lamdera.Wire2.encodeList Lamdera.Wire2.failEncode)


expected_w2_decode_BannedTypeDeepAlias w2_x_c_msg =
    Lamdera.Wire2.decodeDict Lamdera.Wire2.decodeFloat (Lamdera.Wire2.decodeList Lamdera.Wire2.failDecode)


expected_w2_encode_RecordParams w2_x_c_msg =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeList (w2_encode_SubCustom w2_x_c_msg) w2_rec_var0.subCustom ]


expected_w2_decode_RecordParams w2_x_c_msg =
    Lamdera.Wire2.succeedDecode (\subCustom0 -> { subCustom = subCustom0 }) |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeList (w2_decode_SubCustom w2_x_c_msg))


expected_w2_encode_SubCustom w2_x_c_value w2v =
    case w2v of
        SubCustomValue v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_x_c_value v0 ]


expected_w2_decode_SubCustom w2_x_c_value =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode SubCustomValue |> Lamdera.Wire2.andMapDecode w2_x_c_value

                    _ ->
                        Lamdera.Wire2.failDecode
            )
