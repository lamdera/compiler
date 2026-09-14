module Test.Wire_Validate_MultiTvar exposing (..)

{-| Multi-parameter custom type with a validator using its type variables in the
same order they were declared (`Pair a b -> Result String ()`). Must compile.
-}


type Pair a b
    = Pair a b


w3_validate_Pair : Pair a b -> Result String ()
w3_validate_Pair _ =
    Ok ()
