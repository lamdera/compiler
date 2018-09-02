module Transpile.Reserved where


import qualified Language.Haskell.Exts.Simple.Syntax as Hs

import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict

(==>) = (,)

reservedWords = reservedElmWords <> reservedHaskellWords
reservedElmWords = ["if", "then", "else", "case", "of", "let", "in", "type", "module", "where", "import", "exposing", "as", "port"]

reservedHaskellSymbols =
  ["!", "'", "''", "-", "--", "-<", "-<<", "->", "::", ";", "<-", ",", "=", "=>", ">", "?", "#", "*", "@", "[|", "|]", "\\", "_", "`", "{", "}", "{-", "-}", "|", "~"]
reservedHaskellWords =
  [ "as"
  , "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "family"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "mdo"
  , "module"
  , "newtype"
  , "of"
  , "proc"
  , "qualified"
  , "rec"
  , "then"
  , "type"
  , "where"
  ]
