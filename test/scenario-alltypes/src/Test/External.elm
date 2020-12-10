module Test.External exposing (..)

import Lamdera.Wire2


type alias ExternalRecordBasic =
    { int : Int }


type ExternalCustomBasic
    = Custom1
    | Custom2


type ExternalCustomThreaded threadedTvar threadedTvar2
    = AlphabeticallyLast
    | AlphabeticallyFirst threadedTvar2
    | AlphabeticallyKMiddleThreaded threadedTvar


type alias ExternalAliasThreaded threadedTvar =
    SubSubRecordAlias threadedTvar


type alias SubSubRecordAlias threadedTvar =
    { subType : Int
    , tvar : threadedTvar
    }


expected_w2_encode_ExternalRecordBasic =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeInt w2_rec_var0.int ]


expected_w2_decode_ExternalRecordBasic =
    Lamdera.Wire2.succeedDecode (\int0 -> { int = int0 }) |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt


expected_w2_encode_ExternalCustomBasic w2v =
    case w2v of
        Custom1 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]

        Custom2 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]


expected_w2_decode_ExternalCustomBasic =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Custom1

                    1 ->
                        Lamdera.Wire2.succeedDecode Custom2

                    _ ->
                        Lamdera.Wire2.failDecode
            )


expected_w2_encode_ExternalAliasThreaded w2_x_c_threadedTvar =
    w2_encode_SubSubRecordAlias w2_x_c_threadedTvar


expected_w2_decode_ExternalAliasThreaded w2_x_c_threadedTvar =
    w2_decode_SubSubRecordAlias w2_x_c_threadedTvar


expected_w2_encode_SubSubRecordAlias w2_x_c_threadedTvar =
    \w2_rec_var0 -> Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeInt w2_rec_var0.subType, w2_x_c_threadedTvar w2_rec_var0.tvar ]


expected_w2_decode_SubSubRecordAlias w2_x_c_threadedTvar =
    Lamdera.Wire2.succeedDecode (\subType0 tvar0 -> { subType = subType0, tvar = tvar0 }) |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar


expected_w2_encode_ExternalCustomThreaded w2_x_c_threadedTvar w2_x_c_threadedTvar2 w2v =
    case w2v of
        AlphabeticallyFirst v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_x_c_threadedTvar2 v0 ]

        AlphabeticallyKMiddleThreaded v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, w2_x_c_threadedTvar v0 ]

        AlphabeticallyLast ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2 ]


expected_w2_decode_ExternalCustomThreaded w2_x_c_threadedTvar w2_x_c_threadedTvar2 =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode AlphabeticallyFirst |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar2

                    1 ->
                        Lamdera.Wire2.succeedDecode AlphabeticallyKMiddleThreaded |> Lamdera.Wire2.andMapDecode w2_x_c_threadedTvar

                    2 ->
                        Lamdera.Wire2.succeedDecode AlphabeticallyLast

                    _ ->
                        Lamdera.Wire2.failDecode
            )
