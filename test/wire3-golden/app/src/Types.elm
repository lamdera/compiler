module Types exposing (..)

{-| Golden types for the Wire3 format, type hashes and Evergreen snapshots.

Wire3 writes record fields and numbers constructors in name order, and the type
hashes and snapshots order fields and constructors the same way. Each name list
below is declared out of order and mixes name lengths, upper case, digits and
underscores, so the lexical (byte) order differs from both the source order and
a shortest-first order. Any change in name ordering changes the golden output.
-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Other exposing (Ext, Named, Opaque, OtherCtor, OtherRec, Person)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Time


type alias Rec =
    { zeta : Int
    , a : String
    , mid : Float
    , longerName : Bool
    , b2 : Char
    , ab : List Int
    , zz : Maybe Int
    , aaa : ()
    , bA : Order
    , b_ : Int
    , b1 : String
    }


type Ctors
    = Zz
    | A
    | Mid
    | LongerName
    | B2 Int
    | Ab String Float
    | Aaa
    | AB
    | A_b
    | Ba (Maybe Ctors)


type alias Nested =
    { outer : { zInner : Int, aInner : { yy : String, x : Int, xLonger : Float } }
    , rec : Rec
    , list : List { q : Int, pp : String, r : Bool }
    }


type Tree a
    = Leaf
    | Node (Tree a) a (Tree a)


type Choice a b
    = Zed a
    | Al b
    | Middle a b
    | None_


type alias Wrapped a =
    { value : a, zcount : Int, at : List a }


type Forest
    = Forest { kids : List Forest, nm : String, z : Int }


type alias Containers =
    { zTuple : ( Int, String )
    , triple : ( Bool, Float, Ctors )
    , maybe : Maybe Rec
    , result : Result String Ctors
    , resultOk : Result Int (List Ctors)
    , list : List Rec
    , array : Array Ctors
    , dict : Dict String Rec
    , dictTuple : Dict ( Int, String ) Float
    , set : Set String
    , seqDict : SeqDict Rec Int
    , seqDictCtor : SeqDict Ctors (List Ctors)
    , seqSet : SeqSet Ctors
    , seqSetRec : SeqSet Rec
    , bytes : Bytes
    , time : Time.Posix
    , unit : ()
    , order : Order
    }


type alias UsesExt =
    { p : Person
    , q : Named { extra : String, ex : Int }
    , e : Ext { k : Int, kk : Bool }
    }


type alias Cross =
    { other : OtherRec
    , ctor : OtherCtor
    , opaque : Opaque
    , wrapped : Wrapped (Choice Rec Ctors)
    , tree : Tree (Choice Int String)
    }


type alias FrontendModel =
    { zeta : Rec, key : Int, a : Ctors, nested : Nested }


type alias BackendModel =
    { containers : Containers
    , usesExt : UsesExt
    , cross : Cross
    , forest : Forest
    , trees : List (Tree Ctors)
    , choice : Choice Int String
    }


type FrontendMsg
    = FNoop
    | FzMsg Rec
    | FaMsg
    | F (Choice Ctors Nested)


type ToBackend
    = ToBackendZ Ctors
    | ToBackendA (Choice String Int)
    | Tb
    | Tb2 UsesExt


type BackendMsg
    = BNoop
    | Bz Int
    | Bb_ Cross


type ToFrontend
    = TfLonger Nested
    | Tf
    | T Containers
