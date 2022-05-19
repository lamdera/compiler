module Test.Wire_Record_Extensible3_Tricky exposing (..)

import Lamdera.Wire3
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
    { otherConfig
        | attributes : List msg
    }


{-| Again, extensible record alias encoders never actually get used
-}
expected_w3_encode_Config_ : (otherConfig -> Lamdera.Wire3.Encoder) -> (msg -> Lamdera.Wire3.Encoder) -> Config_ otherConfig msg -> Lamdera.Wire3.Encoder
expected_w3_encode_Config_ w3_x_c_otherConfig w3_x_c_msg =
    Lamdera.Wire3.failEncode


expected_w3_decode_Config_ w3_x_c_otherConfig w3_x_c_msg =
    Lamdera.Wire3.failDecode
