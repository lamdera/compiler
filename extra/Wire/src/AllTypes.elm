module AllTypes exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Evergreen as EG
import Json.Decode as D
import Json.Encode as E
import Msg exposing (..)
import Set exposing (Set)
import Time


-- @TODO get aliases of remote types working
-- type alias AliasRemote = Herp
-- After checking everything, including simplifying the import, this is def a problem:
type Referenced
    = Root
    | Wrapped Herp

-- @TODO check this next after custom type is working
-- type alias Referenced =
--     { wrapped : Herp }


type alias AliasInt =
    Int


type alias AliasTuple =
    ( Float, Bool )


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
    | Aliased AliasInt
    | Recursive Union
    | ValueTwoParams Bool Char
    | ValueTuple ( Int, String )
    | ValueResult (Result String Int)


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
