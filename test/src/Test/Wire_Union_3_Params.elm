module Test.Wire_Union_3_Params exposing (..)

import Lamdera.Wire2


type UnionParams
    = ValueStandalone
    | ValueInt Int
    | ValueFloat Float
    | ValueBool Bool
    | ValueChar Char
    | ValueString String
    | ValueOrder Order
    | ValueUnit ()
    | ValueTwoParams Bool Char
    | ValueListBool (List Bool)



-- Values above here are okay
-- | ValueSetFloat (Set Float)
-- | ValueArrayString (Array String)
-- | ValueDict (Dict String (List Int))
-- | ValueTime Time.Posix
-- | ValueAliased AliasInt
-- | ValueRecursive AllUnion
--   -- | ValueSubRecursive (ExternalRecord Int) -- breaks wire
--   -- | ValueSubRecursive (ExternalRecord AllUnion) -- also breaks wire
-- | ValueSubRecursive SubRecursiveRecord -- also breaks wire
-- | ValueDeep Subdir.Subsubdir.SubsubdirType.DeepRec
-- | ValueTuple ( Int, String )
-- | ValueTriple ( Int, String, Bool )
-- | ValueResult (Result String Int)
-- | ValueCustom (ExternalCustom Int)
-- | ValueCustomDeep Subdir.Subsubdir.SubsubdirType.DeepCustom
-- | ValueCustomBasic ExternalCustomBasic
-- | ValuePhantom (Phantom OnlyUsedInPhantom)
-- | ValueAliasTuple ExternalAliasTuple
-- | ValueAll AllTypes


w2_encode_UnionParams w2_e_val =
    case w2_e_val of
        ValueBool v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, Lamdera.Wire2.encodeBool v0 ]

        ValueChar v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, Lamdera.Wire2.encodeChar v0 ]

        ValueFloat v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 2, Lamdera.Wire2.encodeFloat v0 ]

        ValueInt v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 3, Lamdera.Wire2.encodeInt v0 ]

        ValueListBool v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 4, Lamdera.Wire2.encodeList Lamdera.Wire2.encodeBool v0 ]

        ValueOrder v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 5, Lamdera.Wire2.encodeOrder v0 ]

        ValueStandalone ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 6 ]

        ValueString v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 7, Lamdera.Wire2.encodeString v0 ]

        ValueTwoParams v0 v1 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 8, Lamdera.Wire2.encodeBool v0, Lamdera.Wire2.encodeChar v1 ]

        ValueUnit v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 9, Lamdera.Wire2.encodeUnit v0 ]


w2_decode_UnionParams =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2_e_val ->
                case w2_e_val of
                    0 ->
                        Lamdera.Wire2.succeedDecode ValueBool |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBool

                    1 ->
                        Lamdera.Wire2.succeedDecode ValueChar |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeChar

                    2 ->
                        Lamdera.Wire2.succeedDecode ValueFloat |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeFloat

                    3 ->
                        Lamdera.Wire2.succeedDecode ValueInt |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt

                    4 ->
                        Lamdera.Wire2.succeedDecode ValueListBool |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeBool)

                    5 ->
                        Lamdera.Wire2.succeedDecode ValueOrder |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeOrder

                    6 ->
                        Lamdera.Wire2.succeedDecode ValueStandalone

                    7 ->
                        Lamdera.Wire2.succeedDecode ValueString |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeString

                    8 ->
                        Lamdera.Wire2.succeedDecode ValueTwoParams |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeBool |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeChar

                    9 ->
                        Lamdera.Wire2.succeedDecode ValueUnit |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeUnit

                    _ ->
                        Lamdera.Wire2.failDecode
            )
