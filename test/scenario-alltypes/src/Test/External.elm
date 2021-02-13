module Test.External exposing (..)

import Lamdera.Wire3


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


expected_w3_encode_ExternalRecordBasic =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeInt w3_rec_var0.int ]


expected_w3_decode_ExternalRecordBasic =
    Lamdera.Wire3.succeedDecode (\int0 -> { int = int0 }) |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt


expected_w3_encode_ExternalCustomBasic w3v =
    case w3v of
        Custom1 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0 ]

        Custom2 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1 ]


expected_w3_decode_ExternalCustomBasic =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Custom1

                    1 ->
                        Lamdera.Wire3.succeedDecode Custom2

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_ExternalAliasThreaded w3_x_c_threadedTvar =
    w3_encode_SubSubRecordAlias w3_x_c_threadedTvar


expected_w3_decode_ExternalAliasThreaded w3_x_c_threadedTvar =
    w3_decode_SubSubRecordAlias w3_x_c_threadedTvar


expected_w3_encode_SubSubRecordAlias w3_x_c_threadedTvar =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeInt w3_rec_var0.subType, w3_x_c_threadedTvar w3_rec_var0.tvar ]


expected_w3_decode_SubSubRecordAlias w3_x_c_threadedTvar =
    Lamdera.Wire3.succeedDecode (\subType0 tvar0 -> { subType = subType0, tvar = tvar0 }) |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt |> Lamdera.Wire3.andMapDecode w3_x_c_threadedTvar


expected_w3_encode_ExternalCustomThreaded w3_x_c_threadedTvar w3_x_c_threadedTvar2 w3v =
    case w3v of
        AlphabeticallyFirst v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w3_x_c_threadedTvar2 v0 ]

        AlphabeticallyKMiddleThreaded v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1, w3_x_c_threadedTvar v0 ]

        AlphabeticallyLast ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 2 ]


expected_w3_decode_ExternalCustomThreaded w3_x_c_threadedTvar w3_x_c_threadedTvar2 =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode AlphabeticallyFirst |> Lamdera.Wire3.andMapDecode w3_x_c_threadedTvar2

                    1 ->
                        Lamdera.Wire3.succeedDecode AlphabeticallyKMiddleThreaded |> Lamdera.Wire3.andMapDecode w3_x_c_threadedTvar

                    2 ->
                        Lamdera.Wire3.succeedDecode AlphabeticallyLast

                    _ ->
                        Lamdera.Wire3.failDecode
            )
