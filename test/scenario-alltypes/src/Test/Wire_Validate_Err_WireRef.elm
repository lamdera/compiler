module Test.Wire_Validate_Err_WireRef exposing (..)

{-| A module that defines a validator AND references a generated wire function
(here the encoder) in its own top-level code.

Because a validator forces the generated wire functions to be placed *after* the
user's code, this top-level reference would previously crash the compiler with
an internal `Map.!` error during type inference. It should now produce a clear
compile error.
-}


type MyType
    = MyType Int


w3_validate_MyType : MyType -> Result String ()
w3_validate_MyType (MyType n) =
    if n >= 0 then
        Ok ()

    else
        Err "must be non-negative"


encodedDefault =
    w3_encode_MyType (MyType 0)
