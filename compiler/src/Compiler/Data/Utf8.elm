module Compiler.Data.Utf8 exposing
    ( Utf8
    , bUnder256
    , bVeryLong
    , contains
    , empty
    , fromPtr
    , fromSnippet
    , size
    , split
    , startsWith
    , toBuilder
    )

import Compiler.Parse.Primitives as P
import Extra.Data.Binary as B
import Extra.Type.List exposing (TList)



-- UTF-8


type alias Utf8 =
    String



-- EMPTY


empty : Utf8
empty =
    ""



-- SIZE


size : Utf8 -> Int
size =
    String.length



-- CONTAINS


contains : Int -> Utf8 -> Bool
contains word str =
    String.contains (String.fromChar (Char.fromCode word)) str



-- STARTS WITH


startsWith : Utf8 -> Utf8 -> Bool
startsWith =
    String.startsWith



-- SPLIT


split : Int -> Utf8 -> TList Utf8
split divider str =
    String.split (String.fromChar (Char.fromCode divider)) str



-- TO BUILDER


toBuilder : Utf8 -> String
toBuilder =
    identity



-- FROM PTR


fromPtr : String -> Int -> Int -> Utf8
fromPtr src pos end =
    String.slice pos end src



-- FROM SNIPPET


fromSnippet : P.Snippet -> Utf8
fromSnippet (P.Snippet fptr off len _ _) =
    String.slice off (off + len) fptr



-- BINARY


bUnder256 : B.Binary Utf8
bUnder256 =
    B.bStringWith B.bWord8


bVeryLong : B.Binary Utf8
bVeryLong =
    B.bString
