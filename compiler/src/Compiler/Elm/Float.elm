module Compiler.Elm.Float exposing
    ( TFloat
    , bTFloat
    , fromPtr
    )

import Compiler.Data.Utf8 as Utf8
import Extra.Data.Binary as B



-- FLOATS


type alias TFloat =
    Utf8.Utf8



-- HELPERS


fromPtr : String -> Int -> Int -> TFloat
fromPtr =
    Utf8.fromPtr



-- BINARY


bTFloat : B.Binary TFloat
bTFloat =
    Utf8.bUnder256
