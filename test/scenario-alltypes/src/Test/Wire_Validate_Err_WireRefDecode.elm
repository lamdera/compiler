module Test.Wire_Validate_Err_WireRefDecode exposing (..)

{-| Same situation as Wire_Validate_Err_WireRef, but referencing the generated
*decoder* rather than the encoder, to confirm the check is symmetric across both
w3_encode_* and w3_decode_*. Should produce a clear compile error.
-}


type MyType
    = MyType Int


w3_validate_MyType : MyType -> Result String ()
w3_validate_MyType (MyType n) =
    if n >= 0 then
        Ok ()

    else
        Err "must be non-negative"


defaultDecoder =
    w3_decode_MyType
