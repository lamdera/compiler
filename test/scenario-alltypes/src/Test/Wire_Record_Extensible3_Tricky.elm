module Test.Wire_Record_Extensible3_Tricky exposing (..)

import Lamdera.Wire3
import Test.Wire_Alias_2_Record
import Test.Wire_Record_Extensible1_Basic



-- Ensure cross-file extensible record type refs work as normal


type alias Config =
    { overlayColor : Test.Wire_Record_Extensible1_Basic.Color
    }


expected_w3_encode_Config : Config -> Lamdera.Wire3.Encoder
expected_w3_encode_Config =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Test.Wire_Record_Extensible1_Basic.w3_encode_Color w3_rec_var0.overlayColor
            ]


expected_w3_decode_Config =
    Lamdera.Wire3.succeedDecode (\overlayColor0 -> { overlayColor = overlayColor0 })
        |> Lamdera.Wire3.andMapDecode Test.Wire_Record_Extensible1_Basic.w3_decode_Color



{- Test that tricky tvar passing is handled properly

   Essence from datetimepicker-legacy/src/DateTimePicker/Config.elm

    @TODO remove this now?:
   Tests neutered as our generation types are more specific than old Source based ones,
   but the actual test was for a generation failure so this ensures the gen at least type checks

-}


type Type msg
    = DateType (Config_ { name : String } msg)


expected_w3_encode_Type : (msg -> Lamdera.Wire3.Encoder) -> Type msg -> Lamdera.Wire3.Encoder
expected_w3_encode_Type w3_x_c_msg w3v =
    case w3v of
        DateType v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeUnsignedInt8 0
                , (\w3_rec_var0 ->
                    Lamdera.Wire3.encodeSequenceWithoutLength
                        [ Lamdera.Wire3.encodeList w3_x_c_msg w3_rec_var0.attributes
                        , Lamdera.Wire3.encodeString w3_rec_var0.name
                        ]
                  )
                    v0
                ]


expected_w3_decode_Type w3_x_c_msg =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode DateType
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode
                                    (\attributes0 name0 -> { attributes = attributes0, name = name0 })
                                    |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList w3_x_c_msg)
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


type alias Config_ otherConfig msg =
    { otherConfig | attributes : List msg }


{-| Again, extensible record alias encoders never actually get used
-}
expected_w3_encode_Config_ : (otherConfig -> Lamdera.Wire3.Encoder) -> (msg -> Lamdera.Wire3.Encoder) -> Config_ otherConfig msg -> Lamdera.Wire3.Encoder
expected_w3_encode_Config_ w3_x_c_otherConfig w3_x_c_msg =
    Lamdera.Wire3.failEncode


expected_w3_decode_Config_ w3_x_c_otherConfig w3_x_c_msg =
    Lamdera.Wire3.failDecode



-- Double param'd


type alias Record a =
    { a | field : a }


expected_w3_encode_Record : (a -> Lamdera.Wire3.Encoder) -> Record a -> Lamdera.Wire3.Encoder
expected_w3_encode_Record w3_x_c_a =
    Lamdera.Wire3.failEncode


expected_w3_decode_Record w3_x_c_a =
    Lamdera.Wire3.failDecode


type alias DoubleParamed =
    Record { b : String }


expected_w3_encode_DoubleParamed : DoubleParamed -> Lamdera.Wire3.Encoder
expected_w3_encode_DoubleParamed =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeString w3_rec_var0.b
            , (\w3_rec_var1 ->
                Lamdera.Wire3.encodeSequenceWithoutLength
                    [ Lamdera.Wire3.encodeString w3_rec_var1.b ]
              )
                w3_rec_var0.field
            ]


expected_w3_decode_DoubleParamed =
    Lamdera.Wire3.succeedDecode
        (\b0 field0 -> { b = b0, field = field0 })
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.succeedDecode
                (\b0 -> { b = b0 })
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
            )



-- This fails on "error "Used toAnnotation on a type that is not well-formed"" from Type.hs
-- type alias Blah =
--     Record Int
{-




   Aliased extension - also from datetimepicker-legacy/src/DateTimePicker/Config.elm above
-}


type DatePickerType
    = DateType_ (AddedConfig (DatePickerConfig {}))
    | DateTimeType_ (AddedConfig (DatePickerConfig TimePickerConfig))
    | TimeType_ (AddedConfig TimePickerConfig)


type alias AddedConfig otherConfig =
    { otherConfig | attributes : String }


type alias DatePickerConfig otherConfig =
    { otherConfig | allowYearNavigation : Bool }


type alias TimePickerConfig =
    { timePickerType : Int }


expected_w3_encode_DatePickerType : DatePickerType -> Lamdera.Wire3.Encoder
expected_w3_encode_DatePickerType w3v =
    case w3v of
        DateTimeType_ v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeUnsignedInt8 0
                , (\w3_rec_var0 ->
                    Lamdera.Wire3.encodeSequenceWithoutLength
                        [ Lamdera.Wire3.encodeBool w3_rec_var0.allowYearNavigation
                        , Lamdera.Wire3.encodeString w3_rec_var0.attributes
                        , Lamdera.Wire3.encodeInt w3_rec_var0.timePickerType
                        ]
                  )
                    v0
                ]

        DateType_ v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeUnsignedInt8 1
                , (\w3_rec_var0 ->
                    Lamdera.Wire3.encodeSequenceWithoutLength
                        [ Lamdera.Wire3.encodeBool w3_rec_var0.allowYearNavigation
                        , Lamdera.Wire3.encodeString w3_rec_var0.attributes
                        ]
                  )
                    v0
                ]

        TimeType_ v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeUnsignedInt8 2
                , (\w3_rec_var0 ->
                    Lamdera.Wire3.encodeSequenceWithoutLength
                        [ Lamdera.Wire3.encodeString w3_rec_var0.attributes
                        , Lamdera.Wire3.encodeInt w3_rec_var0.timePickerType
                        ]
                  )
                    v0
                ]


expected_w3_decode_DatePickerType =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode DateTimeType_
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode
                                    (\allowYearNavigation0 attributes0 timePickerType0 -> { allowYearNavigation = allowYearNavigation0, attributes = attributes0, timePickerType = timePickerType0 })
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                )

                    1 ->
                        Lamdera.Wire3.succeedDecode DateType_
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode
                                    (\allowYearNavigation0 attributes0 -> { allowYearNavigation = allowYearNavigation0, attributes = attributes0 })
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeBool
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                )

                    2 ->
                        Lamdera.Wire3.succeedDecode TimeType_
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode
                                    (\attributes0 timePickerType0 -> { attributes = attributes0, timePickerType = timePickerType0 })
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_DatePickerConfig : (otherConfig -> Lamdera.Wire3.Encoder) -> (DatePickerConfig otherConfig -> Lamdera.Wire3.Encoder)
expected_w3_encode_DatePickerConfig w3_x_c_otherConfig =
    Lamdera.Wire3.failEncode


expected_w3_decode_DatePickerConfig w3_x_c_otherConfig =
    Lamdera.Wire3.failDecode


expected_w3_encode_AddedConfig : (otherConfig -> Lamdera.Wire3.Encoder) -> (AddedConfig otherConfig -> Lamdera.Wire3.Encoder)
expected_w3_encode_AddedConfig w3_x_c_otherConfig =
    Lamdera.Wire3.failEncode


expected_w3_decode_AddedConfig w3_x_c_otherConfig =
    Lamdera.Wire3.failDecode


expected_w3_encode_TimePickerConfig : TimePickerConfig -> Lamdera.Wire3.Encoder
expected_w3_encode_TimePickerConfig =
    \w3_rec_var0 -> Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeInt w3_rec_var0.timePickerType ]


expected_w3_decode_TimePickerConfig =
    Lamdera.Wire3.succeedDecode (\timePickerType0 -> { timePickerType = timePickerType0 }) |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt