module AllTypes exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type RecursiveUnion
    = Node RecursiveUnion
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
    , dictIntStirng : Dict Int String
    , order : Order
    , union : RecursiveUnion
    }
