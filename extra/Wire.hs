module Wire where

import AST.Source (VarType(..), Type_(..))
import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N


fr =
    Region
      { _start = Position
        { _line   = 12354678
        , _column = 12354678
        }
      , _end   = Position
        { _line   = 12354678
        , _column = 12354678
        }
      }


at = At fr

int x = at (Int x)

name = N.fromString

named n = at (name n)

qval q n = at (VarQual Value (name q) (name n))

ctor n = at (Var Ctor (name "AllTypes"))

call f args = at (Call (f) args)

decl n srcPat expr mSig = at (Decl (named n) srcPat expr mSig)

binops exprs expr = at (Binops exprs expr)


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
    (Just (at (TTypeQual fr (name "D") (name "Decoder") [ at (TType fr (name "AllTypes") []) ])))
