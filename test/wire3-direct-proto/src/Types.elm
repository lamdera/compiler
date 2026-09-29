module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Rec =
    { id : Int
    , score : Float
    , name : String
    , active : Bool
    , note : Maybe String
    , tags : List String
    , counts : List Int
    }


type Shape
    = Circle Float
    | Rect Float Float
    | Empty
    | Labelled String (Maybe Shape)


type Id
    = Id String


type Color
    = Red
    | Green
    | Blue


type Tree
    = Leaf
    | Node Tree Int String Tree


type alias DB a =
    { db : Dict String a
    , seed : Int
    }


type alias Model =
    { recs : Dict String Rec
    , ints : List Int
    , shapes : List Shape
    , ids : Set String
    , idList : List Id
    , colors : List Color
    , tree : Tree
    , db : DB Rec
    , arr : Array Int
    , pair : ( Int, String )
    , res : Result String Int
    , ch : Char
    , unit : ()
    }
