module Test.Wire_Validate_Recursive exposing (..)

{-| A custom type that directly references itself, with a validator.

The generated `w3_decode_Tree` is self-recursive (it decodes sub-Trees by
calling itself) AND it calls `w3_validate_Tree`. This module must compile, which
verifies:

  - codegen terminates (no infinite unfolding of the recursive type)
  - the self-recursive decoder is still grouped correctly when wrapped with the
    validator call
  - the decoder can reference the user `w3_validate_Tree` even though it lives in
    a recursive binding group

Because validation is baked into `w3_decode_Tree`, it runs on every Tree node
decoded (each recursive level), not just the outermost one.
-}


type Tree
    = Leaf Int
    | Branch Tree Tree


w3_validate_Tree : Tree -> Result String ()
w3_validate_Tree tree =
    case tree of
        Leaf n ->
            if n >= 0 then
                Ok ()

            else
                Err "Leaf value must be non-negative"

        Branch _ _ ->
            Ok ()
