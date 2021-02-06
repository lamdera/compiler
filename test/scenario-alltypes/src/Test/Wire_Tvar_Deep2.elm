module Test.Wire_Tvar_Deep2 exposing (..)

import Lamdera.Wire3
import Test.Wire_Tvar_Deep3


type alias Nested ta =
    { types : Test.Wire_Tvar_Deep3.Documented ta
    }
