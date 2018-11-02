module AllTypes exposing (AllTypes, IntAlias, Union(..), evg, evg_d_AllTypes, evg_d_Union, evg_e_AllTypes, evg_e_Union)

import Array exposing (Array)
import Dict exposing (Dict)
import Evergreen as EG
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Time


type alias IntAlias =
    Int


type Union
    = Leaf
    | ValueInt Int
    | ValueFloat Float
    | ValueBool Bool
    | ValueChar Char
    | ValueString String
    | ValueListBool (List Bool)
    | ValueSetFloat (Set Float)
    | ValueArrayString (Array String)
    | ValueDict (Dict String (List Int))
    | ValueTime Time.Posix
    | ValueOrder Order
    | ValueUnit ()
    | Aliased IntAlias
    | Recursive Union


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
