module Test.Wire_Union_4_Tricky exposing (..)

import Array exposing (Array)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Lamdera.Wire3
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


expected_w3_encode_AliasInt : AliasInt -> Lamdera.Wire3.Encoder
expected_w3_encode_AliasInt =
    Lamdera.Wire3.encodeInt


expected_w3_decode_AliasInt =
    Lamdera.Wire3.decodeInt


expected_w3_encode_UnionTricky : UnionTricky -> Lamdera.Wire3.Encoder
expected_w3_encode_UnionTricky w3v =
    case w3v of
        ValueAliased v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, w3_encode_AliasInt v0 ]

        ValueCustom v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 1, Test.External.w3_encode_ExternalCustomThreaded Lamdera.Wire3.encodeInt Lamdera.Wire3.encodeString v0 ]

        ValueRecursive v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 2, w3_encode_UnionTricky v0 ]


expected_w3_decode_UnionTricky =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Bytes.Decode.map
                            (\w3_v1 -> ValueAliased w3_v1)
                            w3_decode_AliasInt

                    1 ->
                        Bytes.Decode.map
                            (\w3_v1 -> ValueCustom w3_v1)
                            (Test.External.w3_decode_ExternalCustomThreaded Lamdera.Wire3.decodeInt Lamdera.Wire3.decodeString)

                    2 ->
                        Bytes.Decode.map
                            (\w3_v1 -> ValueRecursive w3_v1)
                            w3_decode_UnionTricky

                    _ ->
                        Lamdera.Wire3.failDecode
            )
