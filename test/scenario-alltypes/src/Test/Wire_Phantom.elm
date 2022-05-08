module Test.Wire_Phantom exposing (..)

import Lamdera.Wire3
import Test.Wire_Phantom2 exposing (..)



-- elm-explorations/test's mutually recursive types
--
-- type Quantity number units
--     = Quantity number
--
--
-- type Rate dependentUnits independentUnits
--     = Rate dependentUnits independentUnits
--


{-| -}
type alias Density =
    CubicMeters


expected_w3_encode_Density : Density -> Lamdera.Wire3.Encoder
expected_w3_encode_Density =
    Test.Wire_Phantom2.w3_encode_CubicMeters


expected_w3_decode_Density =
    Test.Wire_Phantom2.w3_decode_CubicMeters
