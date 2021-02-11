module Test.Basic exposing (..)


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


test desc bool =
    if bool then
        "✅ " ++ desc

    else
        "❌ " ++ desc


equals v1 v2 =
    v1 == v2


addOne x =
    x + 1


suite =
    test "addOne adds one" <|
        equals (addOne 123) 124
