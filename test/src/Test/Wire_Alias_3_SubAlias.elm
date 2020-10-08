module Test.Wire_Alias_3_SubAlias exposing (..)

import Lamdera.Wire2


type alias AliasThreaded threadedTvar =
    SubRecordAlias threadedTvar


type alias SubRecordAlias threadedTvar =
    { tvar : threadedTvar }


expected_w2_encode_AliasThreaded w2_x_c_threadedTvar =
    w2_encode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_decode_AliasThreaded w2_x_c_threadedTvar =
    w2_decode_SubRecordAlias w2_x_c_threadedTvar


expected_w2_encode_SubRecordAlias w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_SubRecordAlias w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\tvar0 -> { tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar
