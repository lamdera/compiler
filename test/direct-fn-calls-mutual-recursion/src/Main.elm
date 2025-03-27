module Main exposing (main)

import Html exposing (Html)


main : Html msg
main =
    Html.div []
        [ Html.text (a1 1)
        , Html.text (a2 1)
        , Html.text (b1 1 1)
        , Html.text (b2 1 1)
        ]


a1 : Int -> String
a1 =
    a2


a2 : Int -> String
a2 n =
    if n > 0 then
        a1 (n - 1)

    else
        "done"


b1 : Int -> Int -> String
b1 =
    b2


b2 : Int -> Int -> String
b2 m n =
    if n > 0 then
        b1 (m - 1) (n - 1)

    else
        "done"
