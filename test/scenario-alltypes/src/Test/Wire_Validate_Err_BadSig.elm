module Test.Wire_Validate_Err_BadSig exposing (..)

{-| Requirement 2: a w3_validate_ function exists for a real custom type, but its
type signature is not `MyType -> Result String ()` (here it returns Bool). This
should be a compile error.
-}


type Badly
    = BadlyA
    | BadlyB


w3_validate_Badly : Badly -> Bool
w3_validate_Badly _ =
    True
