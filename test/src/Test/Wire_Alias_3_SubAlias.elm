module Test.Wire_Alias_3_SubAlias exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
import Set exposing (Set)
import Time exposing (..)


type alias AliasThreaded threadedTvar =
    SubRecordAlias threadedTvar


type alias SubRecordAlias threadedTvar =
    { tvar : threadedTvar }


type alias RecordThreadedWrap threadedTvar =
    { tvar : Taggers threadedTvar }


type alias Taggers msg =
    Dict.Dict Float (List (Posix -> msg))


expected_w2_encode_RecordThreadedWrap w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ w2_encode_Taggers w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_RecordThreadedWrap w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode (w2_decode_Taggers w2_x_c_threadedTvar)


expected_w2_encode_AliasThreaded w2_x_c_threadedTvar =
    w2_encode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_decode_AliasThreaded w2_x_c_threadedTvar =
    w2_decode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_encode_SubRecordAlias w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_SubRecordAlias w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar


expected_w2_encode_Taggers w2_x_c_msg =
    Lamdera.Wire2.encodeDict Lamdera.Wire2.encodeFloat (Lamdera.Wire2.encodeList Lamdera.Wire2.failEncode)


expected_w2_decode_Taggers w2_x_c_msg =
    Lamdera.Wire2.decodeDict Lamdera.Wire2.decodeFloat (Lamdera.Wire2.decodeList Lamdera.Wire2.failDecode)
