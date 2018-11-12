module Transpiler exposing (T(..))

-- ### changes in 0.19 ###
-- cannot expose specific constructors anymore; it's all `T(..)` or nothing `T`
-- no more variable shadowing, anywhere
-- top-level record destructoring removed; intentional or not?

import Browser
import Html


main =
    Browser.sandbox { init = (), view = \m -> Html.text "asdf", update = \_ model -> model }


type T
    = A
    | B


testuniontype { recordfield } =
    recordfield


testuniontype2 { recordfield } =
    recordfield

tailrec

-- {a} = x -- TODO: this was removed in 19; intentional or not? waiting for evan to answer


f { a, b } =
    a


type Optional a
    = Some a
    | None


inContext { given, message } =
    String.join "\n"
        [ case given of
            Nothing ->
                ""

            Just x ->
                "Just " ++ x
        , message
        ]


tst =
    let
        f3 { a } =
            a
    in
    f3 { a = 1 }



-- shadowing was removed in 19
-- shadowingInLetStmt a =
--   let
--     f4 {a1,b2} = a
--     a1 = 3
--     b2 = 4
--   in f4 {a1=1}


tstNoRecords =
    let
        f5 =
            a

        a =
            3
    in
    f5


f1 { a } =
    a


fx r =
    let
        { a, b } =
            r
    in
    a


potato =
    0.19
