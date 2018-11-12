{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire.Valid where

import AST.Source (VarType(..), Type_(..), Pattern_(..))
import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N
import qualified AST.Valid as AS (Module(..))

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint


stubValid valid flag pkg importDict interfaces source =
  case valid of
    AS.Module n _ _ _ _ _ _ _ _ _ ->
      if N.toString n == "AllTypes" then

        tracef
          ("-" ++ N.toString n)
          (valid { _decls =
            [ evg_e_AllTypes_stubbed
            , evg_d_AllTypes_stubbed
            , evg_e_Union_stubbed
            , evg_d_Union_stubbed
            ] })

      else
        valid


-- Our injection point POC for AllTypes. Search for `Wire.modify`
modify valid flag pkg importDict interfaces source canonical =
  case valid of
    AS.Module n _ _ _ _ _ _ _ _ _ ->
      if N.toString n == "AllTypes_Gen" then
        valid
        -- tracef ("-" ++ N.toString n) valid
        -- tracef ("-" ++ N.toString n) (valid { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })

      else if N.toString n == "AllTypes" then
        -- valid
        tracef
          ("-" ++ N.toString n)
          (valid { _decls =
            [ evg_e_AllTypes
            , evg_d_AllTypes
            , evg_e_Union
            , evg_d_Union
            ] })
        -- tracef ("-" ++ N.toString n) (valid { _decls = _decls valid })
        -- tracef ("-" ++ N.toString n) valid

      else
        valid


-- AST to file debugger
tracef n a =
  unsafePerformIO $ do
    putStrLn ("ast-" ++ n ++ ".txt")
    writeFile ("ast-" ++ n ++ ".txt") $ prettyShow a
    pure a


-- Helpers for the At/Region prefixes
region =
  Region
    { _start = Position { _line   = 10 , _column = 10 }
    , _end   = Position { _line   = 10 , _column = 10 }
    }


at = At region


-- An Int literal
int x = at (Int x)


-- A String literal
str x = at (Str x)


-- Unit () literal
unit = at Unit


-- A symbol name
name = N.fromString


-- A name at a region
named n = at (name n)


-- A qualified value
qvar q n = at (VarQual Value (name q) (name n))


-- A constructor
ctor n = at (Var Ctor (name n))


-- A variable/value
var n = at (Var Value (name n))


-- A function call
call f args = at (Call (f) args)


-- A function declaration
decl n srcPat expr mSig = at (Decl (named n) srcPat expr mSig)


-- A lambda
lambda params expr = at (Lambda params expr)


-- An anything match, i.e. \_ ->
anything = at PAnything


-- A list of expressions joined by operators, such as |>
binops exprs expr = at (Binops exprs expr)


-- A list literal
list exprs = at (List exprs)


-- Record field accessor, i.e. record.field as r.f
field r f = at (Access (var r) (named f))


-- Type signature type
typ n rest = at (TType region (name n) rest)


-- Type signature qualified type
qtyp q n rest = at (TTypeQual region (name q) (name n) rest)


-- Function definition paramater variable
pvar n = at (PVar (name n))


-- Case statement
case_ expr patterns = at (Case expr patterns)


-- Pattern match constructor in case statement
pctor n vars = at (PCtor region (name n) vars)



-- This is a POC AST implementation of the AllTypes encoder & decoder
evg_d_AllTypes =
  decl "evg_d_AllTypes" []
    (binops
      [ (call (qvar "D" "succeed") [ ctor "AllTypes" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 0 , qvar "D" "int" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 1 , qvar "D" "float" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 2 , qvar "D" "bool" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 3 , qvar "EG" "d_char" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 4 , qvar "D" "string" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 5 , call (qvar "D" "list") [ qvar "D" "int" ] ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 6 , call (qvar "EG" "d_set") [ qvar "D" "float" ] ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 7 , call (qvar "D" "array") [ qvar "D" "string" ] ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 8 , call (qvar "EG" "d_dict") [ qvar "D" "string" , call (qvar "D" "list") [ qvar "D" "int" ] ] ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 9 , qvar "EG" "d_time" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 10 , qvar "EG" "d_order" ] , named "|>" )
      , (call (qvar "EG" "atIndex") [ int 11 , var "evg_d_Union" ] , named "|>" ) -- continue numbers here
      ]
      (call (qvar "EG" "atIndex") [ int 12 , call (qvar "D" "null") [ unit ] ])
    )
    (Just (qtyp "D" "Decoder" [typ "AllTypes" []]))


evg_d_AllTypes_stubbed =
  decl "evg_d_AllTypes" []
  (qvar "Debug" "todo")
  (Just (qtyp "D" "Decoder" [typ "AllTypes" []]))


evg_e_AllTypes =
  decl "evg_e_AllTypes" [pvar "evg_p0"]
    (call
      (qvar "E" "list")
      [ var "identity"
      , list
          [ call (qvar "E" "int") [ field "evg_p0" "int" ]
          , call (qvar "E" "float") [ field "evg_p0" "float" ]
          , call (qvar "E" "bool") [ field "evg_p0" "bool" ]
          , call (qvar "EG" "e_char") [ field "evg_p0" "char" ]
          , call (qvar "E" "string") [ field "evg_p0" "string" ]
          , call (qvar "E" "list") [ qvar "E" "int" , field "evg_p0" "listInt" ]
          , call (qvar "E" "set") [ qvar "E" "float" , field "evg_p0" "setFloat" ]
          , call (qvar "E" "array") [ qvar "E" "string" , field "evg_p0" "arrayString" ]
          , call (qvar "EG" "e_dict") [ qvar "E" "string" , call (qvar "E" "list") [qvar "E" "int"] , field "evg_p0" "dict" ]
          , call (qvar "EG" "e_time") [ field "evg_p0" "time" ]
          , call (qvar "EG" "e_order") [ field "evg_p0" "order" ]
          , call (var "evg_e_Union") [ field "evg_p0" "union" ]
          , qvar "E" "null"
          ]
      ]
    )
    -- Not sure why this type signature has a different form from the decoder, but that's what the diffs told us...
    (Just (at (TLambda (typ "AllTypes" []) (qtyp "E" "Value" []))))


evg_e_AllTypes_stubbed =
  decl "evg_e_AllTypes" [pvar "evg_p0"]
  (qvar "Debug" "todo")
  (Just (at (TLambda (typ "AllTypes" []) (qtyp "E" "Value" []))))


evg_e_Union_stubbed =
  decl "evg_e_Union" [pvar "evg_p0"]
  (qvar "Debug" "todo")
  (Just (at (TLambda (typ "Union" []) (qtyp "E" "Value" []))))

evg_e_Union =
  decl "evg_e_Union" [pvar "evg_p0"]
      (case_ (var "evg_p0")
        [ ( pctor "Recursive" [pvar "evg_v0"]
          , call (qvar "E" "list")
            [ var "identity" , list [ call (qvar "E" "string") [ str "Recursive"] , call (var "evg_e_Union") [var "evg_v0"] ] ]
          )
        , ( pctor "Valued" [pvar "evg_v0"]
          , call (qvar "E" "list")
            [ var "identity" , list [ call (qvar "E" "string") [ str "Valued"] , call (qvar "E" "int") [ var "evg_v0" ] ] ]
          )
        , ( pctor "DeeplyValued" [pvar "evg_v0"]
          , call (qvar "E" "list")
            [ var "identity" , list [ call (qvar "E" "string") [ str "DeeplyValued"] , call (qvar "E" "list") [ qvar "E" "bool" , var "evg_v0" ] ] ]
          )
        , ( pctor "Leaf" []
          , call (qvar "E" "list")
            [ var "identity" , list [ call (qvar "E" "string") [ str "Leaf" ] ] ]
          )
        ]
      )
      (Just (at (TLambda (typ "Union" []) (qtyp "E" "Value" []))))


evg_d_Union =
  decl "evg_d_Union" []
    (call (qvar "D" "oneOf")
      [list
        [ call (qvar "EG" "union1") [ str "Recursive" , call (qvar "D" "lazy") [ lambda [ anything ] (var "evg_d_Union") ] , ctor "Recursive" ]
        , call (qvar "EG" "union1") [ str "Valued" , qvar "D" "int" , ctor "Valued" ]
        , call (qvar "EG" "union1") [ str "DeeplyValued" , call (qvar "D" "list") [qvar "D" "bool"] , ctor "DeeplyValued" ]
        , call (qvar "EG" "union") [ str "Leaf" ,ctor "Leaf" ]
        ]
      ]
    )
    (Just (qtyp "D" "Decoder" [typ "Union" [] ]))


evg_d_Union_stubbed =
  decl "evg_d_Union" []
  (qvar "Debug" "todo")
  (Just (qtyp "D" "Decoder" [typ "Union" [] ]))
