module Test.Wire_Validate_Err_ArgCount exposing (..)

{-| Requirement 2 variant: a validator declared as a value rather than a
one-argument function. The signature does not include the type as an argument
at all, so it should be a compile error.
-}


type Z
    = Z


w3_validate_Z : Result String ()
w3_validate_Z =
    Ok ()
