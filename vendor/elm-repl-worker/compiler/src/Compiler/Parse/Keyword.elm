{- MANUALLY FORMATTED -}
module Compiler.Parse.Keyword exposing
  ( type_, alias_, port_
  , if_, then_, else_
  , case_, of_
  , let_, in_
  , infix_, left_, right_, non_
  , module_, import_, exposing_, as_
  , effect_, where_, command_, subscription_
  , k4, k5
  )


import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var



-- DECLARATIONS


type_ : (P.Row -> P.Col -> x) -> P.Parser x ()
type_ tx = k4 0x74 0x79 0x70 0x65 tx

alias_ : (P.Row -> P.Col -> x) -> P.Parser x ()
alias_ tx = k5 0x61 0x6C 0x69 0x61 0x73 tx

port_ : (P.Row -> P.Col -> x) -> P.Parser x ()
port_ tx = k4 0x70 0x6F 0x72 0x74 tx



-- IF EXPRESSIONS


if_ : (P.Row -> P.Col -> x) -> P.Parser x ()
if_ tx = k2 0x69 0x66 tx

then_ : (P.Row -> P.Col -> x) -> P.Parser x ()
then_ tx = k4 0x74 0x68 0x65 0x6E tx

else_ : (P.Row -> P.Col -> x) -> P.Parser x ()
else_ tx = k4 0x65 0x6C 0x73 0x65 tx



-- CASE EXPRESSIONS


case_ : (P.Row -> P.Col -> x) -> P.Parser x ()
case_ tx = k4 0x63 0x61 0x73 0x65 tx

of_ : (P.Row -> P.Col -> x) -> P.Parser x ()
of_ tx = k2 0x6F 0x66 tx



-- LET EXPRESSIONS


let_ : (P.Row -> P.Col -> x) -> P.Parser x ()
let_ tx = k3 0x6C 0x65 0x74 tx

in_ : (P.Row -> P.Col -> x) -> P.Parser x ()
in_ tx = k2 0x69 0x6E tx



-- INFIXES


infix_ : (P.Row -> P.Col -> x) -> P.Parser x ()
infix_ tx = k5 0x69 0x6E 0x66 0x69 0x78 tx

left_ : (P.Row -> P.Col -> x) -> P.Parser x ()
left_ tx = k4 0x6C 0x65 0x66 0x74 tx

right_ : (P.Row -> P.Col -> x) -> P.Parser x ()
right_ tx = k5 0x72 0x69 0x67 0x68 0x74 tx

non_ : (P.Row -> P.Col -> x) -> P.Parser x ()
non_ tx = k3 0x6E 0x6F 0x6E tx



-- IMPORTS


module_ : (P.Row -> P.Col -> x) -> P.Parser x ()
module_ tx = k6 0x6D 0x6F 0x64 0x75 0x6C 0x65 tx

import_ : (P.Row -> P.Col -> x) -> P.Parser x ()
import_ tx = k6 0x69 0x6D 0x70 0x6F 0x72 0x74 tx

exposing_ : (P.Row -> P.Col -> x) -> P.Parser x ()
exposing_ tx = k8 0x65 0x78 0x70 0x6F 0x73 0x69 0x6E 0x67 tx

as_ : (P.Row -> P.Col -> x) -> P.Parser x ()
as_ tx = k2 0x61 0x73 tx



-- EFFECTS


effect_ : (P.Row -> P.Col -> x) -> P.Parser x ()
effect_ tx = k6 0x65 0x66 0x66 0x65 0x63 0x74 tx

where_ : (P.Row -> P.Col -> x) -> P.Parser x ()
where_ tx = k5 0x77 0x68 0x65 0x72 0x65 tx

command_ : (P.Row -> P.Col -> x) -> P.Parser x ()
command_ tx = k7 0x63 0x6F 0x6D 0x6D 0x61 0x6E 0x64 tx

subscription_ : (P.Row -> P.Col -> x) -> P.Parser x ()
subscription_ toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos12 = pos + 12 in
    if pos12 <= end
      && P.unsafeIndex src (pos     ) == 0x73
      && P.unsafeIndex src (pos +  1) == 0x75
      && P.unsafeIndex src (pos +  2) == 0x62
      && P.unsafeIndex src (pos +  3) == 0x73
      && P.unsafeIndex src (pos +  4) == 0x63
      && P.unsafeIndex src (pos +  5) == 0x72
      && P.unsafeIndex src (pos +  6) == 0x69
      && P.unsafeIndex src (pos +  7) == 0x70
      && P.unsafeIndex src (pos +  8) == 0x74
      && P.unsafeIndex src (pos +  9) == 0x69
      && P.unsafeIndex src (pos + 10) == 0x6F
      && P.unsafeIndex src (pos + 11) == 0x6E
      && Var.getInnerWidth src pos12 end == 0
    then
      let s = P.State src pos12 end indent row (col + 12) in P.Cok () s
    else
      P.Eerr row col toError



-- KEYWORDS


k2 : Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k2 w1 w2 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos2 = pos + 2 in
    if pos2 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && Var.getInnerWidth src pos2 end == 0
    then
      let s = P.State src pos2 end indent row (col + 2) in P.Cok () s
    else
      P.Eerr row col toError


k3 : Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k3 w1 w2 w3 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos3 = pos + 3 in
    if pos3 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && Var.getInnerWidth src pos3 end == 0
    then
      let s = P.State src pos3 end indent row (col + 3) in P.Cok () s
    else
      P.Eerr row col toError


k4 : Int -> Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k4 w1 w2 w3 w4 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos4 = pos + 4 in
    if pos4 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && Var.getInnerWidth src pos4 end == 0
    then
      let s = P.State src pos4 end indent row (col + 4) in P.Cok () s
    else
      P.Eerr row col toError


k5 : Int -> Int -> Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k5 w1 w2 w3 w4 w5 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos5 = pos + 5 in
    if pos5 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && P.unsafeIndex src (pos + 4) == w5
      && Var.getInnerWidth src pos5 end == 0
    then
      let s = P.State src pos5 end indent row (col + 5) in P.Cok () s
    else
      P.Eerr row col toError


k6 : Int -> Int -> Int -> Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k6 w1 w2 w3 w4 w5 w6 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos6 = pos + 6 in
    if pos6 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && P.unsafeIndex src (pos + 4) == w5
      && P.unsafeIndex src (pos + 5) == w6
      && Var.getInnerWidth src pos6 end == 0
    then
      let s = P.State src pos6 end indent row (col + 6) in P.Cok () s
    else
      P.Eerr row col toError


k7 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k7 w1 w2 w3 w4 w5 w6 w7 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos7 = pos + 7 in
    if pos7 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && P.unsafeIndex src (pos + 4) == w5
      && P.unsafeIndex src (pos + 5) == w6
      && P.unsafeIndex src (pos + 6) == w7
      && Var.getInnerWidth src pos7 end == 0
    then
      let s = P.State src pos7 end indent row (col + 7) in P.Cok () s
    else
      P.Eerr row col toError


k8 : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (P.Row -> P.Col -> x) -> P.Parser x ()
k8 w1 w2 w3 w4 w5 w6 w7 w8 toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let pos8 = pos + 8 in
    if pos8 <= end
      && P.unsafeIndex src (pos    ) == w1
      && P.unsafeIndex src (pos + 1) == w2
      && P.unsafeIndex src (pos + 2) == w3
      && P.unsafeIndex src (pos + 3) == w4
      && P.unsafeIndex src (pos + 4) == w5
      && P.unsafeIndex src (pos + 5) == w6
      && P.unsafeIndex src (pos + 6) == w7
      && P.unsafeIndex src (pos + 7) == w8
      && Var.getInnerWidth src pos8 end == 0
    then
      let s = P.State src pos8 end indent row (col + 8) in P.Cok () s
    else
      P.Eerr row col toError
