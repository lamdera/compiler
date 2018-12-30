module Transpile.Reserved where


import qualified Language.Haskell.Exts.Simple.Syntax as Hs

import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Elm.Name as N

(==>) :: a -> b -> (a, b)
(==>) = (,)

avoidReservedWord :: String -> String
avoidReservedWord l = if elem l Transpile.Reserved.reservedWords then "l'" <> l else l

ident :: N.Name -> Hs.Name
ident name =
  let
    l = (rawIdent name)
  in
  Hs.Ident $
    if elem l Transpile.Reserved.reservedWords then "l'" <> l else l

symIdent :: N.Name -> Hs.Name
symIdent name =
  Hs.Symbol $
    case rawIdent name of
      "++"           -> "<>"
      "::"           -> ":"
      ":"            -> "::"
      (':':rest)     -> '+' : ':' : rest
      ('+':':':rest) -> '+' : ':' : ':' : rest
      a              -> a

rawIdent :: N.Name -> String
rawIdent name = Text.unpack $ N.toText name

reservedWords :: [String]
reservedWords = reservedElmWords <> reservedHaskellWords

reservedElmWords :: [String]
reservedElmWords = ["if", "then", "else", "case", "of", "let", "in", "type", "module", "where", "import", "exposing", "as", "port"]

reservedHaskellSymbols :: [String]
reservedHaskellSymbols =
  ["!", "'", "''", "-", "--", "-<", "-<<", "->", "::", ";", "<-", ",", "=", "=>", ">", "?", "#", "*", "@", "[|", "|]", "\\", "_", "`", "{", "}", "{-", "-}", "|", "~"]

reservedHaskellWords :: [String]
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
