module Test.Wire_Record_Extensible12_ManyRows_Err_PolyRec exposing (..)

{-| A type variable that is the row tail of more than one record (see
Wire_Record_Extensible12_ManyRows), in a type that refers to itself at other type arguments
(see Wire_Recursive_NonRegular): not supported, a compile error rather than codecs that
don't typecheck.
-}


type Poly a
    = PolyX { a | x : Int }
    | PolyY { a | y : Int }
    | PolyNested (Poly { a | z : Int })
