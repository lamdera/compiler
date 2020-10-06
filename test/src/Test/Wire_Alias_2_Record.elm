module Test.Wire_Alias_2_Record exposing (..)

import Lamdera.Wire2
import Test.External exposing (..)


type alias AllTypes =
    { int : Int
    , float : Float

    -- , bool : Bool
    -- , char : Char
    -- , string : String
    , listInt : List Int

    -- , setFloat : Set Float
    -- , arrayString : Array String
    -- , dict : Dict String (List Int)
    -- , time : Time.Posix
    -- , order : Order
    -- , union : AllUnion
    -- , unit : ()
    , externalCustomBasic : ExternalCustomBasic

    -- , externalCustom : ExternalCustom Int
    }


w2_encode_AllTypes =
    \w2_rec_var0 ->
        Lamdera.Wire2.encodeSequenceWithoutLength
            [ Lamdera.Wire2.encodeInt w2_rec_var0.int
            , Lamdera.Wire2.encodeFloat w2_rec_var0.float
            , Lamdera.Wire2.encodeList Lamdera.Wire2.encodeInt w2_rec_var0.listInt
            , Test.External.w2_encode_ExternalCustomBasic w2_rec_var0.externalCustomBasic
            ]


w2_decode_AllTypes =
    Lamdera.Wire2.succeedDecode (\int0 float0 listInt0 externalCustomBasic0 -> { int = int0, float = float0, listInt = listInt0, externalCustomBasic = externalCustomBasic0 })
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeInt
        |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.decodeFloat
        |> Lamdera.Wire2.andMapDecode (Lamdera.Wire2.decodeList Lamdera.Wire2.decodeInt)
        |> Lamdera.Wire2.andMapDecode Test.External.w2_decode_ExternalCustomBasic
