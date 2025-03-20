module Main exposing (main)

import Html


fn1 x =
    "fn1"


fn2 x y =
    "fn2"


fn3 x y z =
    "fn3"


type Type
    = Ctor1 ()
    | Ctor2 () ()
    | Ctor3 () () ()


type Newtype a
    = N a


type Newtype2 a b
    = N2 a b


main =
    let
        x1 =
            fn1 0

        x2 =
            fn2 0 0

        x3 =
            fn3 0 0 0

        c1 =
            Ctor1 ()

        c2 =
            Ctor2 () ()

        c3 =
            Ctor3 () () ()

        n1 =
            N ()

        n2 =
            N2 () ()
    in
    Html.text "compiles!"
