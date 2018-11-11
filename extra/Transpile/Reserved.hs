module Transpile.Reserved where


import qualified Language.Haskell.Exts.Simple.Syntax as Hs

import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict
import qualified Data.Text as Text
import qualified Elm.Name as N

(==>) = (,)

avoidReservedWord l = if elem l Transpile.Reserved.reservedWords then "l'" <> l else l

ident name =
  let
    l = (rawIdent name)
  in
  Hs.Ident $
    if elem l Transpile.Reserved.reservedWords then "l'" <> l else l

symIdent name =
  Hs.Symbol $
    case rawIdent name of
      "++"           -> "<>"
      "::"           -> ":"
      ":"            -> "::"
      (':':rest)     -> '+' : ':' : rest
      ('+':':':rest) -> '+' : ':' : ':' : rest
      a              -> a
rawIdent name = Text.unpack $ N.toText name


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
