module Test.Wire_Validate_Err_Alias exposing (..)

{-| Requirement 5: a w3_validate_ function exists for a type alias rather than a
custom type. This should be a compile error.
-}


type alias Aliased =
    Int


w3_validate_Aliased : Aliased -> Result String ()
w3_validate_Aliased _ =
    Ok ()
