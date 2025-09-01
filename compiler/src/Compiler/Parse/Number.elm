{- MANUALLY FORMATTED -}
module Compiler.Parse.Number exposing
  ( Number(..)
  , number
  , chompHex
  , precedence
  )


import Compiler.AST.Utils.Binop as Binop
import Compiler.Elm.Float as EF
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Error.Syntax as E



-- HELPERS


isDirtyEnd : Int -> Bool
isDirtyEnd word =
  Var.getInnerWidthHelp word > 0


isDecimalDigit : Int -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}



-- NUMBERS


type Number
  = CInt Int
  | CFloat EF.TFloat


number : (P.Row -> P.Col -> x) -> (E.Number -> P.Row -> P.Col -> x) -> P.Parser x Number
number toExpectation toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos >= end then
      P.Eerr row col toExpectation

    else
      let word = P.unsafeIndex src pos in
      if not (isDecimalDigit word) then
        P.Eerr row col toExpectation

      else
        let
          outcome =
            if word == 0x30 {-0-} then
              chompZero src (pos + 1) end
            else
              chompInt src (pos + 1) end (word - 0x30 {-0-})
        in
          case outcome of
            Err newPos problem ->
              let
                newCol = col + newPos - pos
              in
              P.Cerr row newCol (toError problem)

            OkInt newPos n ->
              let
                newCol = col + newPos - pos
                integer = CInt n
                newState = P.State src newPos end indent row newCol
              in
              P.Cok integer newState

            OkFloat newPos ->
              let
                newCol = col + newPos - pos
                copy = EF.fromPtr src pos newPos
                float = CFloat copy
                newState = P.State src newPos end indent row newCol
              in
              P.Cok float newState



-- CHOMP OUTCOME


-- first Int is newPos
--
type Outcome
  = Err Int E.Number
  | OkInt Int Int
  | OkFloat Int



-- CHOMP INT


chompInt : String -> Int -> Int -> Int -> Outcome
chompInt src pos end n =
  if pos >= end then

    OkInt pos n

  else

    let
      word = P.unsafeIndex src pos
    in
      if isDecimalDigit word then
        chompInt src (pos + 1) end (10 * n + word - 0x30 {-0-})

      else if word == 0x2E {-.-} then
        chompFraction src pos end n

      else if word == 0x65 {-e-} || word == 0x45 {-E-} then
        chompExponent src (pos + 1) end

      else if isDirtyEnd word then
        Err pos E.NumberEnd

      else
        OkInt pos n



-- CHOMP FRACTION


chompFraction : String -> Int -> Int -> Int -> Outcome
chompFraction src pos end n =
  let
    pos1 = pos + 1
  in
  if pos1 >= end then
    Err pos (E.NumberDot n)

  else if isDecimalDigit (P.unsafeIndex src pos1) then
    chompFractionHelp src (pos1 + 1) end

  else
    Err pos (E.NumberDot n)


chompFractionHelp : String -> Int -> Int -> Outcome
chompFractionHelp src pos end =
  if pos >= end then
    OkFloat pos

  else
    let word = P.unsafeIndex src pos in
    if isDecimalDigit word then
      chompFractionHelp src (pos + 1) end

    else if word == 0x65 {-e-} || word == 0x45 {-E-} then
      chompExponent src (pos + 1) end

    else if isDirtyEnd word then
      Err pos E.NumberEnd

    else
      OkFloat pos



-- CHOMP EXPONENT


chompExponent : String -> Int -> Int -> Outcome
chompExponent src pos end =
  if pos >= end then
    Err pos E.NumberEnd

  else
    let word = P.unsafeIndex src pos in
    if isDecimalDigit word then
      chompExponentHelp src (pos + 1) end

    else if word == 0x2B {-+-} || word == 0x2D {---} then

      let pos1 = pos + 1 in
      if pos1 < end && isDecimalDigit (P.unsafeIndex src pos1) then
        chompExponentHelp src (pos + 2) end
      else
        Err pos E.NumberEnd

    else
      Err pos E.NumberEnd


chompExponentHelp : String -> Int -> Int -> Outcome
chompExponentHelp src pos end =
  if pos >= end then
    OkFloat pos

  else if isDecimalDigit (P.unsafeIndex src pos) then
    chompExponentHelp src (pos + 1) end

  else
    OkFloat pos



-- CHOMP ZERO


chompZero : String -> Int -> Int -> Outcome
chompZero src pos end =
  if pos >= end then
    OkInt pos 0

  else
    let word = P.unsafeIndex src pos in
    if word == 0x78 {-x-} then
      chompHexInt src (pos + 1) end

    else if word == 0x2E {-.-} then
      chompFraction src pos end 0

    else if isDecimalDigit word then
      Err pos E.NumberNoLeadingZero

    else if isDirtyEnd word then
      Err pos E.NumberEnd

    else
      OkInt pos 0


chompHexInt : String -> Int -> Int -> Outcome
chompHexInt src pos end =
  let (newPos, answer) = chompHex src pos end in
  if answer < 0 then
    Err newPos E.NumberHexDigit
  else
    OkInt newPos answer



-- CHOMP HEX


-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

chompHex : String -> Int -> Int -> (Int, Int)
chompHex src pos end =
  chompHexHelp src pos end (-1) 0


chompHexHelp : String -> Int -> Int -> Int -> Int -> (Int, Int)
chompHexHelp src pos end answer accumulator =
  if pos >= end then
    (pos, answer)
  else
    let
      newAnswer =
        stepHex (P.unsafeIndex src pos) accumulator
    in
    if newAnswer < 0 then
      (pos, if newAnswer == -1 then answer else -2)
    else
      chompHexHelp src (pos + 1) end newAnswer newAnswer


stepHex : Int -> Int -> Int
stepHex word acc =
  if 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} then 16 * acc + (word - 0x30 {- 0 -})
  else if 0x61 {- a -} <= word && word <= 0x66 {- f -} then 16 * acc + 10 + (word - 0x61 {- a -})
  else if 0x41 {- A -} <= word && word <= 0x46 {- F -} then 16 * acc + 10 + (word - 0x41 {- A -})
  else if isDirtyEnd word then -2
  else -1



-- PRECEDENCE


precedence : (P.Row -> P.Col -> x) -> P.Parser x Binop.Precedence
precedence toExpectation =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos >= end then
      P.Eerr row col toExpectation

    else
      let word = P.unsafeIndex src pos in
      if isDecimalDigit word then
        P.Cok
          (Binop.Precedence (word - 0x30 {-0-}))
          (P.State src (pos + 1) end indent row (col + 1))

      else
        P.Eerr row col toExpectation
