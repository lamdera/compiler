module Test.Wire_Record_Extensible6_TwoLevel exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


{-| Multi-level extensible record chains where an extensible record
extends another extensible record before being concretely filled.

This pattern is used by packages like elm-css where Length extends
LengthOrAutoOrCoverOrContain.
-}


-- Two-level chain: Level2 extends Level1, then Concrete fills it in


type alias Level1 compatible =
    { compatible | level1Field : String }


expected_w3_encode_Level1 : ({ compatible | level1Field : String.String } -> Lamdera.Wire3.Encoder) -> Level1 compatible -> Lamdera.Wire3.Encoder
expected_w3_encode_Level1 w3_x_c_compatible =
    w3_x_c_compatible


expected_w3_decode_Level1 w3_x_c_compatible =
    w3_x_c_compatible


type alias Level2 compatible =
    Level1 { compatible | level2Field : Int }


type alias TwoLevelConcrete =
    Level2 { concreteField : Bool }


expected_w3_encode_TwoLevelConcrete : TwoLevelConcrete -> Lamdera.Wire3.Encoder
expected_w3_encode_TwoLevelConcrete =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeBool w3_rec_var0.concreteField
            , Lamdera.Wire3.encodeString w3_rec_var0.level1Field
            , Lamdera.Wire3.encodeInt w3_rec_var0.level2Field
            ]


expected_w3_decode_TwoLevelConcrete =
    Lamdera.Wire3.succeedDecode
        (\concreteField0 level1Field0 level2Field0 -> { concreteField = concreteField0, level1Field = level1Field0, level2Field = level2Field0 })
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt


-- Three-level chain


type alias Level3 compatible =
    Level2 { compatible | level3Field : Float }


type alias ThreeLevelConcrete =
    Level3 { deepField : Char }


expected_w3_encode_ThreeLevelConcrete : ThreeLevelConcrete -> Lamdera.Wire3.Encoder
expected_w3_encode_ThreeLevelConcrete =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeChar w3_rec_var0.deepField
            , Lamdera.Wire3.encodeString w3_rec_var0.level1Field
            , Lamdera.Wire3.encodeInt w3_rec_var0.level2Field
            , Lamdera.Wire3.encodeFloat w3_rec_var0.level3Field
            ]


expected_w3_decode_ThreeLevelConcrete =
    Lamdera.Wire3.succeedDecode
        (\deepField0 level1Field0 level2Field0 level3Field0 -> { deepField = deepField0, level1Field = level1Field0, level2Field = level2Field0, level3Field = level3Field0 })
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeChar
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat


-- Union wrapping two-level concrete types


type TwoLevelUnion
    = WrapTwoLevel TwoLevelConcrete
    | WrapThreeLevel ThreeLevelConcrete
    | NoWrap


expected_w3_encode_TwoLevelUnion : TwoLevelUnion -> Lamdera.Wire3.Encoder
expected_w3_encode_TwoLevelUnion w3v =
    case w3v of
        NoWrap ->
            Bytes.Encode.unsignedInt8 0

        WrapThreeLevel v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 1, w3_encode_ThreeLevelConcrete v0 ]

        WrapTwoLevel v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 2, w3_encode_TwoLevelConcrete v0 ]

