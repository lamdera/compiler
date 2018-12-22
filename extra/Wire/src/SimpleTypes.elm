module SimpleTypes exposing (..)

import Maybe
import Dict exposing (Dict)

type T
  = TC (List T)


type alias TypeAlias a =
  (a, Int)

type alias ComplexType a b c = a -> (a -> b, b -> c) -> (a, b, c)

type T3 a b c
  = A a (T3 a b c)
  | B b (T3 a b c)
  | C c (T3 a b c)
  | Done

type TX a b =
  TX a b

type TxInt a =
  TxInt (TX Int a)

type Rec b a = Rec { y : Int, z : b, x : a}

type Optional a
    = Some a
    | Nada

a = 3


type Dictionary comparable value =
  Dictionary (List (comparable, value))

type Order
    = LT
    | EQ
    | GT
