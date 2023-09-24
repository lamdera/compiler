module Test.Basic exposing (..)

import Eval


type Test
    = UnitTest (() -> List Expectation)
      -- | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String Test
    | Skipped Test
    | Only Test
    | Batch (List Test)


type Expectation
    = Pass
    | Fail { given : Maybe String, description : String, reason : Reason }


type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName


type CustomType
    = Custom1
    | Custom2
    | CustomParam Int


test desc bool =
    Eval.timed <|
        if bool then
            "✅ " ++ desc

        else
            "❌ " ++ desc


equals v1 v2 =
    v1 == v2


notEquals v1 v2 =
    v1 /= v2


addOne x =
    x + 1


add x y =
    x + y


suite : List String
suite =
    [ test "addOne adds one" <|
        equals (addOne 1) 2
    , test "addOne adds float" <|
        equals (addOne 1.5) 2.5
    , test "List.repeat 0 0" <|
        equals (List.repeat 0 0) []
    , test "List.repeat 2 0" <|
        equals (List.repeat 2 0) [ 0, 0 ]

    --
    , test "List.repeat 10M" <|
        -- Just a performance test
        notEquals (List.repeat 10000000 0) []

    -- , test "folding with a plus" <|
    --     equals (List.foldl (+) 0 [ 1, 2, 3 ]) 6
    , test "Custom type equality" <|
        equals Custom1 Custom1
    , test "Custom type inequality" <|
        notEquals Custom1 Custom2
    , test "Custom type param equality" <|
        equals (CustomParam 123) (CustomParam 123)
    , test "Custom type param inequality" <|
        notEquals (CustomParam 123) (CustomParam 321)
    ]
