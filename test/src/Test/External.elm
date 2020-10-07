module Test.External exposing (..)

import Lamdera.Wire2


type alias ExternalRecordBasic =
    { int : Int }


type ExternalCustomBasic
    = Custom1
    | Custom2



-- type ExternalCustom threadedTvar
--     = AlphabeticallyLast
--     | AlphabeticallyFirst
--     | AlphabeticallyKMiddleThreaded threadedTvar


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
