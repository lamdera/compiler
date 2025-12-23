module Compiler.AST.Utils.Shader exposing
    ( Source
    , Types(..)
    , bSource
    , fromChars
    , toJsStringBuilder
    )

import Compiler.Data.Name as Name
import Extra.Data.Binary as B
import Extra.Type.Set as Set



-- SOURCE


type Source
    = Source String



-- TYPES


type Types
    = Types {- attribute -} (Set.Set Name.Name) {- uniform -} (Set.Set Name.Name) {- varying -} (Set.Set Name.Name)



-- TO BUILDER


toJsStringBuilder : Source -> String
toJsStringBuilder (Source src) =
  src



-- FROM CHARS


fromChars : String -> Source
fromChars chars =
    Source (escape chars)


escape : String -> String
escape chars =
    case String.uncons chars of
        Nothing ->
            ""

        Just ( c, cs ) ->
            if c == '\u{000D}' then
                escape cs

            else if c == '\n' then
                String.cons '\\' (String.cons 'n' (escape cs))

            else if c == '"' then
                String.cons '\\' (String.cons '"' (escape cs))

            else if c == '\'' then
                String.cons '\\' (String.cons '\'' (escape cs))

            else if c == '\\' then
                String.cons '\\' (String.cons '\\' (escape cs))

            else
                String.cons c (escape cs)



-- BINARY


bSource : B.Binary Source
bSource =
    B.bin1 Source (\(Source a) -> a) B.bString
