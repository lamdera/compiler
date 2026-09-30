module Other exposing (..)

-- Types used from another module: parameterised, recursive through a parameter, extensible.


type Rose a
    = Rose a (List (Rose a))


type Box a
    = Box a


type alias Wrap a =
    { inner : a, tag : String }


type Two a b
    = TwoZ
    | TwoB b
    | TwoAB a b
    | TwoA a


type alias Named a =
    { a | name : String }


type Phantom a
    = Phantom String
