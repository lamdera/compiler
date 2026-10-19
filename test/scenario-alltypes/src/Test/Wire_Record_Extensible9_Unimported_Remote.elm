module Test.Wire_Record_Extensible9_Unimported_Remote exposing (..)

{-| See Test.Wire_Record_Extensible9_Unimported. Only the Model module imports this one. -}


type Remote e a
    = NotAsked
    | Failure e
    | Success a


type alias WebData a =
    Remote String a
