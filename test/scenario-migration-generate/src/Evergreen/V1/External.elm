module Evergreen.V1.External exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Time


type ExternalUnion
    = External1
    | External2


type alias AllTypes =
    { int : Int
    , float : Float
    , bool : Bool
    , char : Char
    , string : String
    , maybeBool : Maybe Bool
    , listInt : List Int
    , setFloat : Set Float
    , arrayString : Array String
    , dict : Dict String (List Int)
    , result : Result Int String
    , time : Time.Posix
    , order : Order
    , unit : ()
    }


type alias Paramed a =
    { subtype : a
    , string : String
    }


type alias Paramed2 a b =
    { subtype : a
    , subtype2 : b
    , string : String
    }


type alias ParamedSub x =
    { subtypeParamed : Paramed x
    , string : String
    }
