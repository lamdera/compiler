module Test.Wire_Core_Types exposing (..)

import Lamdera.Wire2
import Time


type CoreTypes
    = ValueTime Time.Posix


expected_w2_encode_CoreTypes w2v =
    case w2v of
        ValueTime v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, Time.w2_encode_Posix v0 ]


expected_w2_decode_CoreTypes =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueTime |> Lamdera.Wire2.andMapDecode Time.w2_decode_Posix

                    _ ->
                        Lamdera.Wire2.failDecode
            )



-- = ValueAliased AliasInt
-- | ValueRecursive UnionTricky
-- | ValueCustom (ExternalCustomThreaded Int String)
-- | ValueStandalone
-- | ValueCustomBasic ExternalCustomBasic
-- | ValueAliased AliasInt
-- Values above here are okay
--
--
--   -- | ValueSubRecursive (ExternalRecord Int) -- breaks wire w mutual recursion
--   -- | ValueSubRecursive (ExternalRecord AllUnion) -- also breaks wire w mutual recursion
-- | ValueSubRecursive SubRecursiveRecord -- also breaks wire
-- | ValueDeep Subdir.Subsubdir.SubsubdirType.DeepRec
-- | ValueCustom (ExternalCustom Int)
-- | ValueCustomDeep Subdir.Subsubdir.SubsubdirType.DeepCustom
-- | ValuePhantom (Phantom OnlyUsedInPhantom)
-- | ValueAliasTuple ExternalAliasTuple
-- | ValueAll AllTypes
