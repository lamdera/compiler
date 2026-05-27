module Test.Wire_Validate_Err_NoAnnotation exposing (..)

{-| Requirement 2: a w3_validate_ function exists for a real custom type, but has
no type annotation. We require the annotation so the signature can be verified,
so this should be a compile error.
-}


type Annotless
    = AnnotlessA
    | AnnotlessB


w3_validate_Annotless _ =
    Ok ()
