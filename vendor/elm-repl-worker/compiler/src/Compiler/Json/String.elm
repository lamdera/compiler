{- MANUALLY FORMATTED -}
module Compiler.Json.String exposing
  ( TString
  --, isEmpty
  --
  , fromPtr
  , fromName
  , fromChars
  , fromSnippet
  --, fromComment
  --
  , toChars
  , toBuilder
  )


import Compiler.Data.Name as Name
import Compiler.Data.Utf8 as Utf8
import Compiler.Parse.Primitives as P



-- JSON STRINGS


-- INVARIANT: any Json.String is appropriately escaped already
-- PERF: is this the right representation for Json.String? Maybe ByteString instead?
--
type alias TString =
  Utf8.Utf8



-- FROM


fromPtr : String -> Int -> Int -> String
fromPtr =
  Utf8.fromPtr


fromChars : TString -> TString
fromChars =
  identity


fromSnippet : P.Snippet -> String
fromSnippet =
  Utf8.fromSnippet


fromName : Name.Name -> String
fromName =
  identity



-- TO


toChars : String -> String
toChars =
  identity


toBuilder : String -> String
toBuilder =
  identity
