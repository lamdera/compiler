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


-- deep recursion through a wide record (cold-depth oracle)


type Fat3
    = Fat3 { c001 : Int, c002 : Int, c003 : Int, c004 : Int, c005 : Int, c006 : Int, c007 : Int, c008 : Int, c009 : Int, c010 : Int, c011 : Int, c012 : Int, c013 : Int, c014 : Int, c015 : Int, c016 : Int, c017 : Int, c018 : Int, c019 : Int, c020 : Int, c021 : Int, c022 : Int, c023 : Int, c024 : Int, c025 : Int, c026 : Int, c027 : Int, c028 : Int, c029 : Int, c030 : Int, c031 : Int, c032 : Int, c033 : Int, c034 : Int, c035 : Int, c036 : Int, c037 : Int, c038 : Int, c039 : Int, c040 : Int, c041 : Int, c042 : Int, c043 : Int, c044 : Int, c045 : Int, c046 : Int, c047 : Int, c048 : Int, c049 : Int, c050 : Int, c051 : Int, c052 : Int, c053 : Int, c054 : Int, c055 : Int, c056 : Int, c057 : Int, c058 : Int, c059 : Int, c060 : Int, c061 : Int, c062 : Int, c063 : Int, c064 : Int, c065 : Int, c066 : Int, c067 : Int, c068 : Int, c069 : Int, c070 : Int, c071 : Int, c072 : Int, c073 : Int, c074 : Int, c075 : Int, c076 : Int, c077 : Int, c078 : Int, c079 : Int, c080 : Int, c081 : Int, c082 : Int, c083 : Int, c084 : Int, c085 : Int, c086 : Int, c087 : Int, c088 : Int, c089 : Int, c090 : Int, c091 : Int, c092 : Int, c093 : Int, c094 : Int, c095 : Int, c096 : Int, c097 : Int, c098 : Int, c099 : Int, c100 : Int, c101 : Int, c102 : Int, c103 : Int, c104 : Int, c105 : Int, c106 : Int, c107 : Int, c108 : Int, c109 : Int, c110 : Int, c111 : Int, c112 : Int, c113 : Int, c114 : Int, c115 : Int, c116 : Int, c117 : Int, c118 : Int, c119 : Int, c120 : Int, c121 : Int, c122 : Int, c123 : Int, c124 : Int, c125 : Int, c126 : Int, c127 : Int, c128 : Int, c129 : Int, c130 : Int, c131 : Int, c132 : Int, c133 : Int, c134 : Int, c135 : Int, c136 : Int, c137 : Int, c138 : Int, c139 : Int, c140 : Int, c141 : Int, c142 : Int, c143 : Int, c144 : Int, c145 : Int, c146 : Int, c147 : Int, c148 : Int, c149 : Int, c150 : Int, c151 : Int, c152 : Int, c153 : Int, c154 : Int, c155 : Int, c156 : Int, c157 : Int, c158 : Int, c159 : Int, c160 : Int, c161 : Int, c162 : Int, c163 : Int, c164 : Int, c165 : Int, c166 : Int, c167 : Int, c168 : Int, c169 : Int, c170 : Int, c171 : Int, c172 : Int, c173 : Int, c174 : Int, c175 : Int, c176 : Int, c177 : Int, c178 : Int, c179 : Int, c180 : Int, c181 : Int, c182 : Int, c183 : Int, c184 : Int, c185 : Int, c186 : Int, c187 : Int, c188 : Int, c189 : Int, c190 : Int, c191 : Int, c192 : Int, c193 : Int, c194 : Int, c195 : Int, c196 : Int, c197 : Int, c198 : Int, c199 : Int, c200 : Int, next : Fat3 }
    | Fat3End


type Fat
    = Fat { a01 : Int, a02 : Int, a03 : Int, a04 : Int, a05 : Int, a06 : Int, a07 : Int, a08 : Int, a09 : Int, a10 : Int, a11 : Int, a12 : Int, a13 : Int, a14 : Int, a15 : Int, a16 : Int, a17 : Int, a18 : Int, a19 : Int, a20 : Int, a21 : Int, a22 : Int, a23 : Int, a24 : Int, a25 : Int, a26 : Int, a27 : Int, a28 : Int, a29 : Int, a30 : Int, a31 : Int, a32 : Int, a33 : Int, a34 : Int, a35 : Int, a36 : Int, a37 : Int, a38 : Int, a39 : Int, a40 : Int, a41 : Int, a42 : Int, a43 : Int, a44 : Int, a45 : Int, a46 : Int, a47 : Int, a48 : Int, a49 : Int, a50 : Int, a51 : Int, a52 : Int, a53 : Int, a54 : Int, a55 : Int, a56 : Int, a57 : Int, a58 : Int, a59 : Int, a60 : Int, next : Fat }
    | FatEnd


type alias SNaN =
    { d : Dict Int Float, k : Dict Float Int, s : Set Float }
