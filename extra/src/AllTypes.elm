module AllTypes exposing (..)

import Array exposing (Array)
import Char
import Dict exposing (Dict)
import Set exposing (Set)
import Time


-- type RecursiveUnion
--     = Node RecursiveUnion
--     | Leaf


type alias AllTypes =
    { int : Int
    , float : Float
    , bool : Bool
    , char : Char
    , string : String
    , listInt : List Int
    , setFloat : Set Float
    , arrayString : Array String
    , dictIntString : Dict Int String
    , order : Order

    -- , union : RecursiveUnion
    -- , time : Time.Posix
    }
