module Wire where

import AST.Source (VarType(..), Type_(..))
import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N


fr =
  Region
    { _start = Position { _line   = 12354678 , _column = 12354678 }
    , _end   = Position { _line   = 12354678 , _column = 12354678 }
    }

at = At fr

int x = at (Int x)

name = N.fromString

named n = at (name n)

qval q n = at (VarQual Value (name q) (name n))

ctor n = at (Var Ctor (name n))

var n = at (Var Value (name n))

call f args = at (Call (f) args)

decl n srcPat expr mSig = at (Decl (named n) srcPat expr mSig)

binops exprs expr = at (Binops exprs expr)

list exprs = at (List exprs)

field r f = at (Access (var r) (named f))

typ n rest = at (TType fr (name n) rest)

qtyp q n rest = at (TTypeQual fr (name q) (name n) rest)


derp =
  decl "evg_d_AllTypes" []
    (binops
      [ (call (qval "D" "succeed") [ ctor "AllTypes" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 0 , qval "D" "int" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 1 , qval "D" "float" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 2 , qval "D" "bool" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 3 , qval "EG" "d_Char" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 4 , qval "D" "string" ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 5 , call (qval "D" "list") [ qval "D" "int" ] ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 6 , call (qval "EG" "d_set") [ qval "D" "float" ] ] , named "|>" )
      , (call (qval "EG" "atIndex") [ int 7 , call (qval "D" "array") [ qval "D" "string" ] ] , named "|>" )
      ]
      (call (qval "EG" "atIndex") [ int 8 , qval "EG" "d_Order" ])
    )
    (Just (qtyp "D" "Decoder" [typ "AllTypes" []]))


derp2 =
  decl "evg_e_AllTypes" []
    (call
      (qval "E" "list")
      [ var "identity"
      , list
          [ call (qval "E" "int") [ field "evg_p0" "int" ]
          , call (qval "E" "float") [ field "evg_p0" "float" ]
          , call (qval "E" "bool") [ field "evg_p0" "bool" ]
          , call (qval "EG" "e_Char") [ field "evg_p0" "char" ]
          , call (qval "E" "string") [ field "evg_p0" "string" ]
          , call (qval "E" "list") [ call (qval "E" "int") [ field "evg_p0" "listInt" ] ]
          , call (qval "E" "set") [ call (qval "E" "float") [ field "evg_p0" "setFloat" ] ]
          , call (qval "E" "array") [ call (qval "E" "string") [ field "evg_p0" "arrayString" ] ]
          , call (qval "EG" "e_Order") [ field "evg_p0" "order" ]
          ]
      ]
    )
    (Just (typ "AllTypes" [qtyp "E" "Value" []]))
