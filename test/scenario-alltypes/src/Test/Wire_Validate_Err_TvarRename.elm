module Test.Wire_Validate_Err_TvarRename exposing (..)

{-| Requirement 3 variant: `type Holder a = ...` is parameterised by `a`, so the
validator must say `Holder a`. Using a different (alpha-renamed) variable like
`Holder b` should be a compile error under strict tvar-name matching.
-}


type Holder a
    = Holder a
    | NoHold


w3_validate_Holder : Holder b -> Result String ()
w3_validate_Holder _ =
    Ok ()
