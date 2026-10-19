module Test.Wire_Record_Extensible9_Unimported_Model exposing (..)

{-| See Test.Wire_Record_Extensible9_Unimported. -}

import Test.Wire_Record_Extensible9_Unimported_Remote exposing (Remote, WebData)


type alias Model m =
    { m
        | pipelines : WebData (List Int)
        , remote : Remote Bool String
    }
