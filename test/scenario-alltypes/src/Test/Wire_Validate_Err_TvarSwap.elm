module Test.Wire_Validate_Err_TvarSwap exposing (..)

{-| Requirement 3 variant: `type Pair a b = ...` has type variables `a` and `b`
in that order. The validator must use them in the same order. Swapping them
(`Pair b a`) should be a compile error.
-}


type Pair a b
    = Pair a b


w3_validate_Pair : Pair b a -> Result String ()
w3_validate_Pair _ =
    Ok ()
