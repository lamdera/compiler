module Main_ exposing (main)

{-| Reaches the generated codecs, as the Lamdera harness does, so code generation runs for them -}

import Html
import Types


main =
    Html.text (Debug.toString ( Types.w3_encode_BackendModel, Types.w3_decode_BackendModel ))
