{- MANUALLY FORMATTED -}
module Compiler.Data.Bag exposing
  ( Bag(..)
  , empty
  , one
  , append
  --, map
  , toList
  --, fromList
  )


import Extra.Type.List exposing (TList)



-- BAGS


type Bag a
  = Empty
  | One a
  | Two (Bag a) (Bag a)



-- HELPERS


empty : Bag a
empty =
  Empty


one : a -> Bag a
one =
  One


append : Bag a -> Bag a -> Bag a
append left right =
  case (left, right) of
    (other, Empty) ->
      other

    (Empty, other) ->
      other

    _ ->
      Two left right



-- TO LIST


toList : Bag a -> TList a
toList bag =
  toListHelp bag []


toListHelp : Bag a -> TList a -> TList a
toListHelp bag list =
  case bag of
    Empty ->
      list

    One x ->
      x :: list

    Two a b ->
      toListHelp a (toListHelp b list)
