module Main exposing (main)

import Html


naiveMap : (a -> b) -> List a -> List b
naiveMap fn list =
    naiveMapHelper fn list []


naiveMapHelper : (a -> b) -> List a -> List b -> List b
naiveMapHelper fn list acc =
    case list of
        [] ->
            List.reverse acc

        x :: xs ->
            naiveMapHelper fn xs (fn x :: acc)

main =
    [ 1, 2, 3, 4 ]
        |> naiveMap String.fromInt
        |> String.join ","
        |> Html.text