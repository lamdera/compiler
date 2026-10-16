module Test.Wire_Validate_Err_WrongOkType exposing (..)

{-| Requirement 2 variant: the validator returns a `Result` but with the wrong
"Ok" payload type (`Int` instead of `()`). Should be a compile error.
-}


type Pickle
    = PickleA
    | PickleB


w3_validate_Pickle : Pickle -> Result String Int
w3_validate_Pickle _ =
    Ok 0
