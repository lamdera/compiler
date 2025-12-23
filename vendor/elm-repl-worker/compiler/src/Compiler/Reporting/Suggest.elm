module Compiler.Reporting.Suggest exposing
    ( distance
    , sort
    )

import Extra.Type.List as MList exposing (TList)
import Levenshtein



-- DISTANCE


distance : String -> String -> Int
distance x y =
    Levenshtein.distance x y



-- SORT


sort : String -> (a -> String) -> TList a -> TList a
sort target toString values =
    MList.sortOn (distance (String.toLower target) << String.toLower << toString) values
