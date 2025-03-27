module Main exposing (main)

import Html exposing (Html)


main : Html msg
main =
    Html.div []
        [ Html.text (a1 3 4 5)
        , Html.text (a2 1 2 3 4 5)
        ]


a1 : Int -> Int -> Int -> String
a1 =
    a2 1 2


a2 : Int -> Int -> Int -> Int -> Int -> String
a2 x1 x2 x3 x4 x5 =
    if True then
        "done"

    else
        a1 3 4 5
