{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire where

import AST.Source (VarType(..), Type_(..), Pattern_(..))
import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N
import qualified AST.Valid as AS (Module(..))

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint


-- Our injection point POC for AllTypes. Search for `Wire.modify`
modify valid =
  case valid of
    AS.Module n _ _ _ _ _ _ _ _ _ ->
      if N.toString n == "AllTypes_Gen" then

        debugAst_ ("-" ++ N.toString n) (valid { _decls = [ encoder, decoder] })
        -- x_ ("-" ++ show n) valid

      else
        valid


-- AST to file debugger
debugAst_ n a =
  unsafePerformIO $ do
    putStrLn ("ast-" ++ n ++ ".txt")
    writeFile ("ast-" ++ n ++ ".txt") $ prettyShow a
    pure a


-- Helpers for the At/Region prefixes
fr =
  Region
    { _start = Position { _line   = 10 , _column = 10 }
    , _end   = Position { _line   = 10 , _column = 10 }
    }


at = At fr


-- Am integer literal
int x = at (Int x)


-- A symbol name
name = N.fromString


-- A name at a region
named n = at (name n)


-- A qualified value
qval q n = at (VarQual Value (name q) (name n))


-- A constructor
ctor n = at (Var Ctor (name n))


-- A variable/value
var n = at (Var Value (name n))


-- A function call
call f args = at (Call (f) args)


-- A function declaration
decl n srcPat expr mSig = at (Decl (named n) srcPat expr mSig)


-- A list of expressions joined by operators, such as |>
binops exprs expr = at (Binops exprs expr)


-- A list literal
list exprs = at (List exprs)


-- Record field accessor, i.e. record.field as r.f
field r f = at (Access (var r) (named f))


-- Type signature type
typ n rest = at (TType fr (name n) rest)


-- Type signature qualified type
qtyp q n rest = at (TTypeQual fr (name q) (name n) rest)


-- Function definition paramater variable
pvar n = at (PVar (name n))


-- This is a POC AST implementation of the AllTypes encoder & decoder
decoder =
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


encoder =
  decl "evg_e_AllTypes" [pvar "evg_p0"]
    (call
      (qval "E" "list")
      [ var "identity"
      , list
          [ call (qval "E" "int") [ field "evg_p0" "int" ]
          , call (qval "E" "float") [ field "evg_p0" "float" ]
          , call (qval "E" "bool") [ field "evg_p0" "bool" ]
          , call (qval "EG" "e_Char") [ field "evg_p0" "char" ]
          , call (qval "E" "string") [ field "evg_p0" "string" ]
          , call (qval "E" "list") [ qval "E" "int" , field "evg_p0" "listInt" ]
          , call (qval "E" "set") [ qval "E" "float" , field "evg_p0" "setFloat" ]
          , call (qval "E" "array") [ qval "E" "string" , field "evg_p0" "arrayString" ]
          , call (qval "EG" "e_Order") [ field "evg_p0" "order" ]
          ]
      ]
    )
    -- Not sure why this type signature has a different form from the decoder, but that's what the diffs told us...
    (Just (at (TLambda (typ "AllTypes" []) (qtyp "E" "Value" []))))
