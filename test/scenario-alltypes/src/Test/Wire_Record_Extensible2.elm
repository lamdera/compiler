module Test.Wire_Record_Extensible2 exposing (..)

import Lamdera.Wire2



-- From elm/css:Css.Internal.elm


type alias ColorValue compatible =
    { compatible | value : String }


type alias Color =
    ColorValue { red : Int, green : Int, blue : Int, alpha : Float }
