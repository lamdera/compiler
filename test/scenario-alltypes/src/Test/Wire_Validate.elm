module Test.Wire_Validate exposing (..)

{-| Custom types that opt in to post-decode validation via a
`w3_validate_<TypeName> : <TypeName> -> Result String ()` function defined in
this module. The generated `w3_decode_<TypeName>` should call it: decoding
succeeds iff the validator returns `Ok ()`, otherwise it logs and fails.

This module must compile, which proves the generated decoders that call the
validators are well-typed (covers requirement 4, plus the valid plain-tvar case
of requirement 3 via `Box a`). The `number`-constrained case lives in
Test.Wire_Validate_Number.
-}


type Validated
    = ValidatedInt Int
    | ValidatedString String


w3_validate_Validated : Validated -> Result String ()
w3_validate_Validated v =
    case v of
        ValidatedInt n ->
            if n >= 0 then
                Ok ()

            else
                Err "ValidatedInt must be non-negative"

        ValidatedString s ->
            if s /= "" then
                Ok ()

            else
                Err "ValidatedString must not be empty"


{-| Requirement 3: `type Box a = ...` validated with `Box a -> Result String ()`.
-}
type Box a
    = Box a
    | EmptyBox


w3_validate_Box : Box a -> Result String ()
w3_validate_Box _ =
    Ok ()


{-| A type *without* a validator is unaffected: its decoder is generated as
before, with no validation call.
-}
type Unvalidated
    = UnvalidatedA
    | UnvalidatedB Int


{-| Nested usage: `Container`'s generated decoder calls `w3_decode_Validated`,
which itself runs validation. So validation runs wherever the type is decoded,
including as a field of another type. (`Container` has no validator of its own.)
-}
type alias Container =
    { item : Validated
    , items : List Validated
    }
