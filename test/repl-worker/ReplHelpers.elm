module ReplHelpers exposing (Point, Shape(..), area, classify, double, origin, triple)

-- A project module for test/repl-worker: the REPL compiles it itself, then mixes
-- it with the packages' compiled artifacts.


type Shape
    = Circle Float
    | Rect Float Float


type alias Point =
    { x : Int, y : Int }


double : Int -> Int
double n =
    n * 2


area : Shape -> Float
area shape =
    case shape of
        Circle r ->
            3 * r * r

        Rect w h ->
            w * h


origin : Point
origin =
    { x = 0, y = 0 }


classify : Char -> String
classify c =
    case c of
        'a' ->
            "a"

        '\n' ->
            "newline"

        _ ->
            "other"


triple : a -> ( a, a, a )
triple v =
    ( v, v, v )
