module Test.Wire_Alias_3_SubAlias exposing (..)

import Array exposing (Array)
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


expected_w2_encode_AliasThreaded w2_x_c_threadedTvar =
    w2_encode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_decode_AliasThreaded w2_x_c_threadedTvar =
    w2_decode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_encode_SubRecordAlias w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_SubRecordAlias w2_x_c_threadedTvar =
    Lamdera.Wire3.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire3.andMapDecode w2_x_c_threadedTvar


expected_w2_encode_RecordThreadedWrap w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ w2_encode_BannedTypeDeepAlias w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_RecordThreadedWrap w2_x_c_threadedTvar =
    Lamdera.Wire3.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire3.andMapDecode (w2_decode_BannedTypeDeepAlias w2_x_c_threadedTvar)


expected_w2_encode_BannedTypeDeepAlias w2_x_c_msg =
    Lamdera.Wire3.encodeDict Lamdera.Wire3.encodeFloat (Lamdera.Wire3.encodeList Lamdera.Wire3.failEncode)


expected_w2_decode_BannedTypeDeepAlias w2_x_c_msg =
    Lamdera.Wire3.decodeDict Lamdera.Wire3.decodeFloat (Lamdera.Wire3.decodeList Lamdera.Wire3.failDecode)


expected_w2_encode_RecordParams w2_x_c_msg =
    \w2_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeList (w2_encode_SubCustom w2_x_c_msg) w2_rec_var0.subCustom ]


expected_w2_decode_RecordParams w2_x_c_msg =
    Lamdera.Wire3.succeedDecode (\subCustom0 -> { subCustom = subCustom0 }) |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList (w2_decode_SubCustom w2_x_c_msg))


expected_w2_encode_SubCustom w2_x_c_value w2v =
    case w2v of
        SubCustomValue v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w2_x_c_value v0 ]


expected_w2_decode_SubCustom w2_x_c_value =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode SubCustomValue |> Lamdera.Wire3.andMapDecode w2_x_c_value

                    _ ->
                        Lamdera.Wire3.failDecode
            )
