module AllTypes exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
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
