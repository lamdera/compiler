module Extra.Type.String exposing
    ( dropWhile
    , takeWhile
    )


dropWhile : (Char -> Bool) -> String -> String
dropWhile predicate string =
    case String.uncons string of
        Nothing ->
            string

        Just ( char, rest ) ->
            if predicate char then
                dropWhile predicate rest

            else
                string


takeWhile : (Char -> Bool) -> String -> String
takeWhile predicate string =
    let
        go : String -> String -> String
        go rest acc =
            case String.uncons rest of
                Nothing ->
                    acc

                Just ( char, rest_ ) ->
                    if predicate char then
                        go rest_ (acc ++ String.fromChar char)

                    else
                        acc
    in
    go string ""
