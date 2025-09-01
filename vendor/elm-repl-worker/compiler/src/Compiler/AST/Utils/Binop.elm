module Compiler.AST.Utils.Binop exposing
    ( Associativity(..)
    , Precedence(..)
    , bAssociativity
    , bPrecedence
    , toInt
    )

import Extra.Data.Binary as B



-- BINOP STUFF


type Precedence
    = Precedence Int


type Associativity
    = Left
    | Non
    | Right



-- HELPER


toInt : Precedence -> Int
toInt (Precedence n) =
    n



-- BINARY


bPrecedence : B.Binary Precedence
bPrecedence =
    B.bin1 Precedence (\(Precedence n) -> n) B.bWord64


bAssociativity : B.Binary Associativity
bAssociativity =
    B.enum "Error reading valid associativity from serialized string"
        [ Left, Non, Right ]
