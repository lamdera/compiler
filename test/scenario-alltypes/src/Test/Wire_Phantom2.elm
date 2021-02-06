module Test.Wire_Phantom2 exposing (..)

import Lamdera.Wire3



-- elm-explorations/test's mutually recursive types


type alias CubicMeters =
    Cubed String


type alias Cubed units =
    Product (Product units units) units


type Product units1 units2
    = Product units1 units2
