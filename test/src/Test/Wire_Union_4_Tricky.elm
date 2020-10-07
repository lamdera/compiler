module Test.Wire_Union_4_Tricky exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
import Set exposing (Set)
import Test.External exposing (..)


type AliasInt
    = Int


type UnionTricky
    = ValueAliased AliasInt
    | ValueRecursive UnionTricky



-- | ValueStandalone
-- | ValueCustomBasic ExternalCustomBasic
-- | ValueAliased AliasInt
-- Values above here are okay
--
--
--
--
--
-- | ValueTime Time.Posix
--   -- | ValueSubRecursive (ExternalRecord Int) -- breaks wire
--   -- | ValueSubRecursive (ExternalRecord AllUnion) -- also breaks wire
-- | ValueSubRecursive SubRecursiveRecord -- also breaks wire
-- | ValueDeep Subdir.Subsubdir.SubsubdirType.DeepRec
-- | ValueCustom (ExternalCustom Int)
-- | ValueCustomDeep Subdir.Subsubdir.SubsubdirType.DeepCustom
-- | ValuePhantom (Phantom OnlyUsedInPhantom)
-- | ValueAliasTuple ExternalAliasTuple
-- | ValueAll AllTypes


expected_w2_encode_AliasInt w2v =
    case w2v of
        Int ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0 ]


expected_w2_decode_AliasInt =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Int

                    _ ->
                        Lamdera.Wire2.failDecode
            )


expected_w2_encode_UnionTricky w2v =
    case w2v of
        ValueAliased v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_encode_AliasInt v0 ]

        ValueRecursive v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, w2_encode_UnionTricky v0 ]


expected_w2_decode_UnionTricky =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueAliased |> Lamdera.Wire2.andMapDecode w2_decode_AliasInt

                    1 ->
                        Lamdera.Wire2.succeedDecode ValueRecursive |> Lamdera.Wire2.andMapDecode w2_decode_UnionTricky

                    _ ->
                        Lamdera.Wire2.failDecode
            )
