module Test.Wire_Validate_Err_TvarConcrete exposing (..)

{-| Requirement 3: `type Holder a = ...` is parameterised by a type variable, so
its validator must be `Holder a -> Result String ()`. Using a concrete type
argument (`Holder Int`) instead should be a compile error.
-}


type Holder a
    = Holder a
    | NoHold


w3_validate_Holder : Holder Int -> Result String ()
w3_validate_Holder _ =
    Ok ()
