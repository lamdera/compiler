module Params exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Other exposing (..)
import Set exposing (Set)


type P0 a
    = P0C0 (( Int, List (Maybe (Box (Int))), Box (Int) )) (Order)


type P1 a b
    = P1C0 (()) ({ zf1 : { zf1 : Int, af1 : Char }, af1 : Set Int }) (b)
    | P1End


type P2 a b
    = P2C0 (Maybe ((P1 (Array (Order)) ((P1 (a) (String)))))) ({ zf2 : a, af2 : Float })
    | P2C1 (Result (Bool) (Bool)) (List (Maybe (Box (Array (List (Char)))))) (List (( a, Float )))


type P3
    = P3C0 (()) (( Int, Float ))


type P4 a b
    = P4C0 (b) (Char)
    | P4C1 (a) (List (Maybe (Box (String)))) (List (Maybe (Box (Order))))


type P5
    = P5C0 (()) (Two (Int) (Float))
    | P5C1 (Int) (List (Maybe (Box (Order))))


type P6 a b
    = P6C0 (List (Maybe (Box (Order)))) (Wrap ((P2 (a) (Bool)))) (List (Maybe (Box ({ zf6 : Int, af6 : String }))))
    | P6C1 (Box (Two (Char) (Order))) (Set Int) (Result (Wrap (Bool)) (Char))


type P7 a
    = P7C0 (Box (Float)) (()) (Maybe (Char))
    | P7C1


type P8 a b
    = P8C0 (Two (List (Maybe (Box (( Int, Float, Float ))))) (Wrap (Dict Int (Bool)))) (List (Maybe (Box (Float))))
    | P8End


type P9 a
    = P9C0 (Float) (Maybe (Wrap (Float)))


type P10 a
    = P10C0 ((P1 (Int) (Float)))
    | P10C1 (String)
    | P10C2 (Two (String) (Int))
    | P10C3 (( a, Order, Order )) (List (Maybe (Box (List (Maybe (Box (Float))))))) (Bool)


type P11 a b
    = P11C0 ((P2 (Int) (List (Maybe (Box (Order))))))
    | P11C1 ({ zf11 : Wrap (Box (Char)), af11 : Array (String) })
    | P11C2 (Bool)
    | P11C3


type P12 a b
    = P12C0 (Maybe ((P11 (( Int, Int )) (Box (String))))) (Dict Int ({ zf12 : (P6 (a) (Char)), af12 : Set String }))


type P13 a
    = P13C0 (String) (a) (List (Maybe (Box (( List (Maybe (Box (Int))), P3 )))))
    | P13End


w3_validate_P0 : P0 a -> Result String ()
w3_validate_P0 v =
    Ok ()


w3_validate_P1 : P1 a b -> Result String ()
w3_validate_P1 v =
    Ok ()


w3_validate_P2 : P2 a b -> Result String ()
w3_validate_P2 v =
    case v of
        P2C1 _ _ _ ->
            Err "P2C1 rejected"

        _ ->
            Ok ()


w3_validate_P3 : P3 -> Result String ()
w3_validate_P3 v =
    Ok ()


w3_validate_P4 : P4 a b -> Result String ()
w3_validate_P4 v =
    case v of
        P4C1 _ _ _ ->
            Err "P4C1 rejected"

        _ ->
            Ok ()


w3_validate_P6 : P6 a b -> Result String ()
w3_validate_P6 v =
    Ok ()


w3_validate_P8 : P8 a b -> Result String ()
w3_validate_P8 v =
    case v of
        P8End ->
            Err "P8End rejected"

        _ ->
            Ok ()


w3_validate_P10 : P10 a -> Result String ()
w3_validate_P10 v =
    case v of
        P10C1 _ ->
            Err "P10C1 rejected"

        _ ->
            Ok ()


w3_validate_P11 : P11 a b -> Result String ()
w3_validate_P11 v =
    Ok ()


w3_validate_P12 : P12 a b -> Result String ()
w3_validate_P12 v =
    Ok ()


type alias IP0_0 = P0 (List (Order))
type alias IP0_1 = P0 (Order)
type alias IP0_2 = P0 (Result (Float) (Two (Order) (Char)))
type alias IP1_0 = P1 (Bool) (Two (Wrap (Order)) (Set String))
type alias IP1_1 = P1 (List (Order)) (( List (Maybe (Box (Char))), Two (Bool) (Char), Char ))
type alias IP1_2 = P1 (Char) (Wrap (Char))
type alias IP2_0 = P2 (Dict Int ({ zf99 : Order, af99 : Int })) (Result (Bool) (String))
type alias IP2_1 = P2 (Char) (Float)
type alias IP2_2 = P2 (Two (Float) (List (Maybe (Box (Float))))) (Char)
type alias IP3_0 = P3
type alias IP4_0 = P4 (Int) (( ( Bool, Float ), ( String, Float, Order ) ))
type alias IP4_1 = P4 (List (Maybe (Box (Maybe (Order))))) (Box ((P8 (Float) (String))))
type alias IP4_2 = P4 ({ zf99 : Int, af99 : Char }) (List (Maybe (Box (String))))
type alias IP5_0 = P5
type alias IP6_0 = P6 (Order) (Char)
type alias IP6_1 = P6 (Result (Float) (String)) (Order)
type alias IP6_2 = P6 (P3) (Order)
type alias IP7_0 = P7 (Set Int)
type alias IP7_1 = P7 (Set String)
type alias IP7_2 = P7 (Char)
type alias IP8_0 = P8 (String) (Order)
type alias IP8_1 = P8 (List (Maybe (Box (Box (Int))))) (Bool)
type alias IP8_2 = P8 (( String, Bool )) (( Box (String), Maybe (Int), Wrap (Char) ))
type alias IP9_0 = P9 ((P2 (String) (Bool)))
type alias IP9_1 = P9 (Char)
type alias IP9_2 = P9 (Float)
type alias IP10_0 = P10 (Order)
type alias IP10_1 = P10 (Maybe (Char))
type alias IP10_2 = P10 (Float)
type alias IP11_0 = P11 (String) (String)
type alias IP11_1 = P11 (( P5, Box (Int), ( Char, Order ) )) (Array (( Char, Order, Float )))
type alias IP11_2 = P11 (Dict Int (Set Int)) (Int)
type alias IP12_0 = P12 (Char) (Set String)
type alias IP12_1 = P12 (Int) ((P12 (Int) (Float)))
type alias IP12_2 = P12 (Int) (Float)
type alias IP13_0 = P13 (Box (Order))
type alias IP13_1 = P13 (String)
type alias IP13_2 = P13 (Wrap ((P9 (Bool))))


type alias PModel =
    { m0 : P0 (List (Order))
    , m1 : P0 (Order)
    , m2 : P0 (Result (Float) (Two (Order) (Char)))
    , m3 : P1 (Bool) (Two (Wrap (Order)) (Set String))
    , m4 : P1 (List (Order)) (( List (Maybe (Box (Char))), Two (Bool) (Char), Char ))
    , m5 : P1 (Char) (Wrap (Char))
    , m6 : P2 (Dict Int ({ zf99 : Order, af99 : Int })) (Result (Bool) (String))
    , m7 : P2 (Char) (Float)
    , m8 : P2 (Two (Float) (List (Maybe (Box (Float))))) (Char)
    , m9 : P3
    , m10 : P4 (Int) (( ( Bool, Float ), ( String, Float, Order ) ))
    , m11 : P4 (List (Maybe (Box (Maybe (Order))))) (Box ((P8 (Float) (String))))
    , m12 : P4 ({ zf99 : Int, af99 : Char }) (List (Maybe (Box (String))))
    , m13 : P5
    , m14 : P6 (Order) (Char)
    , m15 : P6 (Result (Float) (String)) (Order)
    , m16 : P6 (P3) (Order)
    , m17 : P7 (Set Int)
    , m18 : P7 (Set String)
    , m19 : P7 (Char)
    , m20 : P8 (String) (Order)
    , m21 : P8 (List (Maybe (Box (Box (Int))))) (Bool)
    , m22 : P8 (( String, Bool )) (( Box (String), Maybe (Int), Wrap (Char) ))
    , m23 : P9 ((P2 (String) (Bool)))
    , m24 : P9 (Char)
    , m25 : P9 (Float)
    , m26 : P10 (Order)
    , m27 : P10 (Maybe (Char))
    , m28 : P10 (Float)
    , m29 : P11 (String) (String)
    , m30 : P11 (( P5, Box (Int), ( Char, Order ) )) (Array (( Char, Order, Float )))
    , m31 : P11 (Dict Int (Set Int)) (Int)
    , m32 : P12 (Char) (Set String)
    , m33 : P12 (Int) ((P12 (Int) (Float)))
    , m34 : P12 (Int) (Float)
    , m35 : P13 (Box (Order))
    , m36 : P13 (String)
    , m37 : P13 (Wrap ((P9 (Bool))))
    }
