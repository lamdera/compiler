module Test.Wire_Validate_Err_NoType exposing (..)

{-| Requirement 1: a w3_validate_ function exists but there is no custom type of
that name defined in this module. (No annotation is given, so canonicalization
doesn't fail first on an unknown type reference — the dedicated wire error
should fire instead.)
-}


w3_validate_Ghost _ =
    Ok ()
