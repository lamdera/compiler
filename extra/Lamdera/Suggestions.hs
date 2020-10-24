{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Suggestions where

{- Helper to remove wire generations from suggestions such as:


-- UNKNOWN EXPORT ---------------------------------------- src/Menu/Internal.elm

You are trying to expose a value named `viewConfig` but I cannot find its
definition.

These names seem close though:

    w2_decode_SectionConfig
    w2_encode_SectionConfig
    w2_decode_SectionNode
    w2_encode_SectionNode

-}

import qualified Data.List as List
import qualified Data.Name as Name
import Lamdera


hideWireSuggestions suggestions =
  suggestions
    -- & debugHaskell "suggestions"
    & filter (\x -> not $ List.isInfixOf "w2_" x)
    & filter (\x -> not $ List.isInfixOf "evg_" x)


hideWireSuggestionsName suggestions =
  suggestions
    -- & debugHaskellPass "suggestions" (suggestions & fmap Name.toChars)
    & filter (\x -> not $ List.isInfixOf "w2_" $ Name.toChars x)
    & filter (\x -> not $ List.isInfixOf "evg_" $ Name.toChars x)
