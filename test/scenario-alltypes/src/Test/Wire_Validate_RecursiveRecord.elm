module Test.Wire_Validate_RecursiveRecord exposing (..)

{-| A custom type that references a record type which in turn references the
custom type: `Node` -> `NodeData` -> `List Node`.

The generated decoders are mutually recursive (`w3_decode_Node` and
`w3_decode_NodeData` reference each other), and `w3_decode_Node` additionally
calls `w3_validate_Node`. This module must compile, verifying the mutually
recursive binding group is formed correctly while still resolving the reference
to the user `w3_validate_Node`.

`NodeData` is a plain record alias with no validator of its own, so its decoder
is generated unchanged.
-}


type Node
    = Node NodeData


type alias NodeData =
    { value : Int
    , children : List Node
    }


w3_validate_Node : Node -> Result String ()
w3_validate_Node (Node data) =
    if data.value >= 0 then
        Ok ()

    else
        Err "Node value must be non-negative"
