module Compiler.Data.OneOrMore exposing
    ( OneOrMore(..)
    , destruct
    , getFirstTwo
    , more
    , one
    )


import Extra.Type.List exposing (TList)



-- ONE OR MORE


type OneOrMore a
    = One a
    | More (OneOrMore a) (OneOrMore a)


one : a -> OneOrMore a
one =
    One


more : OneOrMore a -> OneOrMore a -> OneOrMore a
more =
    More



-- DESTRUCT


destruct : (a -> TList a -> b) -> OneOrMore a -> b
destruct func oneOrMore =
    destructLeft func oneOrMore []


destructLeft : (a -> TList a -> b) -> OneOrMore a -> TList a -> b
destructLeft func oneOrMore xs =
    case oneOrMore of
        One x ->
            func x xs

        More a b ->
            destructLeft func a (destructRight b xs)


destructRight : OneOrMore a -> TList a -> TList a
destructRight oneOrMore xs =
    case oneOrMore of
        One x ->
            x :: xs

        More a b ->
            destructRight a (destructRight b xs)



-- GET FIRST TWO


getFirstTwo : OneOrMore a -> OneOrMore a -> ( a, a )
getFirstTwo left right =
    case left of
        One x ->
            ( x, getFirstOne right )

        More lleft lright ->
            getFirstTwo lleft lright


getFirstOne : OneOrMore a -> a
getFirstOne oneOrMore =
    case oneOrMore of
        One x ->
            x

        More left _ ->
            getFirstOne left
