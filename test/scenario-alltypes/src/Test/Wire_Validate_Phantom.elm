module Test.Wire_Validate_Phantom exposing (..)

{-| A phantom-type variable: `Phantom` declares `a` but no constructor uses it.
The validator signature still has to use `a` to match the type's declared
variables (`Phantom a -> Result String ()`). Must compile.
-}


type Phantom a
    = Phantom


w3_validate_Phantom : Phantom a -> Result String ()
w3_validate_Phantom _ =
    Ok ()
