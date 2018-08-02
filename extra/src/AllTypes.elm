module AllTypes exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Evergreen as EG
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Time


type Union
    = Recursive Union
    | Valued Int
    | DeeplyValued (List Bool)
    | Leaf


type alias AllTypes =
    { int : Int
    , float : Float
    , bool : Bool
    , char : Char
    , string : String
    , listInt : List Int
    , setFloat : Set Float
    , arrayString : Array String
    , dict : Dict String (List Int)
    , time : Time.Posix
    , order : Order
    , union : Union
    , unit : ()
    }


evg_e_Union : Union -> E.Value
evg_e_Union evg_p0 =
    case evg_p0 of
        Recursive evg_v0 ->
            E.list identity [ E.string "Recursive", evg_e_Union evg_v0 ]

        Valued evg_v0 ->
            E.list identity [ E.string "Valued", E.int evg_v0 ]

        DeeplyValued evg_v0 ->
            E.list identity [ E.string "DeeplyValued", E.list E.bool evg_v0 ]

        Leaf ->
            E.list identity [ E.string "Leaf" ]


evg_d_Union : D.Decoder Union
evg_d_Union =
    D.oneOf
        [ EG.union1 "Recursive" (D.lazy (\_ -> evg_d_Union)) Recursive
        , EG.union1 "Valued" D.int Valued
        , EG.union1 "DeeplyValued" (D.list D.bool) DeeplyValued
        , EG.union "Leaf" Leaf
        ]


evg_e_AllTypes : AllTypes -> E.Value
evg_e_AllTypes evg_p0 =
    E.list identity
        [ E.int evg_p0.int
        , E.float evg_p0.float
        , E.bool evg_p0.bool
        , EG.e_char evg_p0.char
        , E.string evg_p0.string
        , E.list E.int evg_p0.listInt
        , E.set E.float evg_p0.setFloat
        , E.array E.string evg_p0.arrayString
        , EG.e_dict E.string (E.list E.int) evg_p0.dict
        , EG.e_time evg_p0.time
        , EG.e_order evg_p0.order
        , evg_e_Union evg_p0.union
        , E.null
        ]


evg_d_AllTypes : D.Decoder AllTypes
evg_d_AllTypes =
    D.succeed AllTypes
        |> EG.atIndex 0 D.int
        |> EG.atIndex 1 D.float
        |> EG.atIndex 2 D.bool
        |> EG.atIndex 3 EG.d_char
        |> EG.atIndex 4 D.string
        |> EG.atIndex 5 (D.list D.int)
        |> EG.atIndex 6 (EG.d_set D.float)
        |> EG.atIndex 7 (D.array D.string)
        |> EG.atIndex 8 (EG.d_dict D.string (D.list D.int))
        |> EG.atIndex 9 EG.d_time
        |> EG.atIndex 10 EG.d_order
        |> EG.atIndex 11 evg_d_Union
        |> EG.atIndex 12 (D.null ())
