module Test.Wire_Record_Extensible12_ManyRows_Other exposing (..)

{-| The foreign half of Wire_Record_Extensible12_ManyRows: a type whose parameter is the row
tail of two records, and an alias for one of them, used from the other module.
-}


type alias Player a =
    { a | status : Int, moves : List Int }


type Other a
    = OtherRow { a | x : Int }
    | OtherPlayer (Player a)
