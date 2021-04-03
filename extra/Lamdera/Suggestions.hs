{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Suggestions where

{- Helper to remove wire generations from suggestions such as:


-- UNKNOWN EXPORT ---------------------------------------- src/Menu/Internal.elm

You are trying to expose a value named `viewConfig` but I cannot find its
definition.

These names seem close though:

    w3_decode_SectionConfig
    w3_encode_SectionConfig
    w3_decode_SectionNode
    w3_encode_SectionNode

-}

import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Map as Map

import qualified Reporting.Annotation as A

import Lamdera


hideWireSuggestions suggestions =
  suggestions
    -- & debugHaskell "suggestions"
    & filter (\x ->
      (not $ List.isPrefixOf "w3_" x) && (not $ List.isPrefixOf "w2_" x)
    )

hideWireSuggestionsName suggestions =
  suggestions
    & filter (\x ->
      (not $ List.isPrefixOf "w3_" $ Name.toChars x) && (not $ List.isPrefixOf "w2_" $ Name.toChars x)
    )

hideWireExports exports =
  exports
    & Map.filterWithKey (\x _ ->
      (not $ List.isPrefixOf "w3_" $ Name.toChars x) && (not $ List.isPrefixOf "w2_" $ Name.toChars x)
    )
