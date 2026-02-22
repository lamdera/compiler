module Lamdera.String (fromChars) where

import qualified Data.Utf8 as Utf8
import qualified Elm.String as ES


fromChars :: [Char] -> ES.String
fromChars =
  Utf8.fromChars . escape


escape :: [Char] -> [Char]
escape =
  foldr
    (\c acc ->
      case c of
        '\\' -> '\\' : '\\' : acc
        '\'' -> '\\' : '\'' : acc
        '\n' -> '\\' : 'n'  : acc
        '\r' -> '\\' : 'r'  : acc
        _    -> c : acc
    )
    []
