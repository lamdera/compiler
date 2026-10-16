module Test.Wire_Validate_Number exposing (..)

{-| Requirement 3: `type Numeric number = ...` is parameterised by a constrained
type variable, so its validator must use the same constrained variable:
`Numeric number -> Result String ()`. This module must compile.
-}


type Numeric number
    = Numeric number


w3_validate_Numeric : Numeric number -> Result String ()
w3_validate_Numeric _ =
    Ok ()
