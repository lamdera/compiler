module Other exposing (..)

{-| Types in a second module, reached from Types.elm through aliases, extensible
records and type parameters. Every name list here is declared out of lexical order
and mixes name lengths, so any change in how names are ordered changes the output.
-}

import Dict exposing (Dict)


type alias Named r =
    { r | zid : Int, name : String, a : Float }


type alias Person =
    Named { longest : String, age : Int, b : Bool }


type alias Ext r =
    { r | zz : List Int, m : Maybe String }


type OtherCtor
    = Zulu
    | Al Int
    | MiddleOne String Bool
    | B
    | Bb (List OtherCtor)


type alias OtherRec =
    { zebra : OtherCtor
    , ab : Dict String Int
    , abc : ( Int, Float )
    , a : Maybe OtherRec2
    }


type alias OtherRec2 =
    { yy : Int, x : String }


type Opaque
    = Opaque { inner : Int, a : String }
