module Types exposing (..)

import Array exposing (Array)
import Big exposing (Big)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Random
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Time


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


type Tagged a
    = Tagged String a
    | Untagged a


type Pair2 a b
    = Pair2 a b (List ( a, b ))


type Phantom a
    = Phantom String


type Rose a
    = Rose a (List (Rose a))



-- mutually recursive


type Forest
    = Forest (List TreeB)
    | NoForest


type TreeB
    = TreeB Int Forest



-- extensible records


type alias Named a =
    { a | name : String }


type alias Person =
    Named { age : Int }


type alias WithPos a =
    { a | x : Float, y : Float }


type Lazy a
    = Lazy (() -> a)
    | Evaluated a


type Prims
    = PrimOrder Order
    | PrimBool Bool
    | PrimChar Char
    | PrimUnit ()
    | PrimTriple ( Int, String, Float )
    | PrimResult (Result Int String)
    | PrimNever (Maybe Never)
    | PrimNested (List (Maybe (Result String (Dict Int Bool))))


type alias Model =
    { recs : Dict String Rec
    , ints : List Int
    , floats : List Float
    , shapes : List Shape
    , ids : Set String
    , idList : List Id
    , colors : List Color
    , tree : Tree
    , db : DB Rec
    , dbNested : DB (List (Maybe Color))
    , arr : Array Int
    , pair : ( Int, String )
    , res : Result String Int
    , ch : Char
    , unit : ()
    , tagged : List (Tagged (DB Int))
    , pair2 : Pair2 Id (Tagged Color)
    , phantom : Phantom Tree
    , rose : Rose (Maybe Int)
    , forest : Forest
    , person : Person
    , positioned : WithPos { label : String }
    , lazy : List (Lazy Int)
    , prims : List Prims
    , time : Time.Posix
    , times : List Time.Posix
    , bytes : Bytes
    , seed : Random.Seed
    , seqDict : SeqDict String (List Int)
    , seqSet : SeqSet Int
    , big : List Big
    , tupleKeys : Dict ( Int, String ) Float
    , anon : { inner : { deep : List Int }, flag : Bool }
    }


type alias WithFn =
    { f : Int -> Int, n : Int }


type HasFn
    = HasFn WithFn
    | NoFn Int
