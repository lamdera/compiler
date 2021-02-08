module Test.Basic exposing (..)


addOne x =
    x + 1



-- suite =
--     describe "addOne"
--         [ test "addOne" <|
--             \_ ->
--                 equal (addOne 123) 124
--         ]


suite =
    -- test "addOne" <|
    -- \_ ->
    addOne 123 == 124


blah =
    True


type Mario
    = Mario


type Dancing a
    = Dancing a


gimmeTheAnnotationMan something =
    case something of
        Mario ->
            Dancing Mario
