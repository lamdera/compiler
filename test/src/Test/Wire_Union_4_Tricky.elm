module Test.Wire_Union_4_Tricky exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
import Set exposing (Set)
import Test.External exposing (..)


type alias AliasInt =
    Int


type UnionTricky
    = ValueAliased AliasInt
    | ValueRecursive UnionTricky
    | ValueCustom (ExternalCustomThreaded Int String)



-- | ValueStandalone
-- | ValueCustomBasic ExternalCustomBasic
-- | ValueAliased AliasInt
-- Values above here are okay
--
--
-- | ValueTime Time.Posix
--   -- | ValueSubRecursive (ExternalRecord Int) -- breaks wire w mutual recursion
--   -- | ValueSubRecursive (ExternalRecord AllUnion) -- also breaks wire w mutual recursion
-- | ValueSubRecursive SubRecursiveRecord -- also breaks wire
-- | ValueDeep Subdir.Subsubdir.SubsubdirType.DeepRec
-- | ValueCustom (ExternalCustom Int)
-- | ValueCustomDeep Subdir.Subsubdir.SubsubdirType.DeepCustom
-- | ValuePhantom (Phantom OnlyUsedInPhantom)
-- | ValueAliasTuple ExternalAliasTuple
-- | ValueAll AllTypes


expected_w2_encode_AliasInt =
    Lamdera.Wire2.encodeInt


expected_w2_decode_AliasInt =
    Lamdera.Wire2.decodeInt


expected_w2_encode_UnionTricky w2v =
    case w2v of
        ValueAliased v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_encode_AliasInt v0 ]

        ValueCustom v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, Test.External.w2_encode_ExternalCustomThreaded Lamdera.Wire2.encodeInt Lamdera.Wire2.encodeString v0 ]

        ValueRecursive v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2, w2_encode_UnionTricky v0 ]


expected_w2_decode_UnionTricky =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueAliased |> Lamdera.Wire2.andMapDecode w2_decode_AliasInt

                    1 ->
                        Lamdera.Wire2.succeedDecode ValueCustom |> Lamdera.Wire2.andMapDecode (Test.External.w2_decode_ExternalCustomThreaded Lamdera.Wire2.decodeInt Lamdera.Wire2.decodeString)

                    2 ->
                        Lamdera.Wire2.succeedDecode ValueRecursive |> Lamdera.Wire2.andMapDecode w2_decode_UnionTricky

                    _ ->
                        Lamdera.Wire2.failDecode
            )
