{- MANUALLY FORMATTED -}
module Compiler.Parse.Variable exposing
  ( lower
  , upper
  , moduleName
  , Upper(..)
  , foreignUpper
  , foreignAlpha
  , chompInnerChars
  , getUpperWidth
  , getInnerWidth
  , getInnerWidthHelp
  , reservedWords
  --
  , isUpper
  , isLower
  , isInner
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Primitives as P
import Extra.Type.Set as Set
import Unicode as UChar



-- LOCAL UPPER


upper : (P.Row -> P.Col -> x) -> P.Parser x Name.Name
upper toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let (newPos, newCol) = chompUpper src pos end col in
    if pos == newPos then
      P.Eerr row col toError
    else
      let name = Name.fromPtr src pos newPos in
      P.Cok name (P.State src newPos end indent row newCol)



-- LOCAL LOWER


lower : (P.Row -> P.Col -> x) -> P.Parser x Name.Name
lower toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let (newPos, newCol) = chompLower src pos end col in
    if pos == newPos then
      P.Eerr row col toError
    else
      let name = Name.fromPtr src pos newPos in
      if Set.member name reservedWords then
        P.Eerr row col toError
      else
        let
          newState =
            P.State src newPos end indent row newCol
        in
        P.Cok name newState


reservedWords : Set.Set Name.Name  -- PERF try using a trie instead
reservedWords =
  Set.fromList
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "exposing"
    , "as"
    , "port"
    ]



-- MODULE NAME


moduleName : (P.Row -> P.Col -> x) -> P.Parser x Name.Name
moduleName toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      (pos1, col1) = chompUpper src pos end col
    in
    if pos == pos1 then
      P.Eerr row col toError
    else
      let
        (status, newPos, newCol) = moduleNameHelp src pos1 end col1
      in
      case status of
        Good ->
          let
            name = Name.fromPtr src pos newPos
            newState = P.State src newPos end indent row newCol
          in
          P.Cok name newState

        Bad ->
          P.Cerr row newCol toError


type ModuleNameStatus
  = Good
  | Bad


moduleNameHelp : String -> Int -> Int -> P.Col -> (ModuleNameStatus, Int, P.Col)
moduleNameHelp src pos end col =
  if isDot src pos end then
    let
      pos1 = pos + 1
      (newPos, newCol) = chompUpper src pos1 end (col + 1)
    in
    if pos1 == newPos then
      (Bad, newPos, newCol)
    else
      moduleNameHelp src newPos end newCol

  else
    (Good, pos, col)



-- FOREIGN UPPER


type Upper
  = Unqualified Name.Name
  | Qualified Name.Name Name.Name


foreignUpper : (P.Row -> P.Col -> x) -> P.Parser x Upper
foreignUpper toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let (upperStart, upperEnd, newCol) = foreignUpperHelp src pos end col in
    if upperStart == upperEnd then
      P.Eerr row newCol toError
    else
      let
        newState = P.State src upperEnd end indent row newCol
        name = Name.fromPtr src upperStart upperEnd
        upperName =
          if upperStart == pos then
            Unqualified name
          else
            let home = Name.fromPtr src pos (upperStart - 1) in
            Qualified home name
      in
      P.Cok upperName newState


foreignUpperHelp : String -> Int -> Int -> P.Col -> (Int, Int, P.Col)
foreignUpperHelp src pos end col =
  let
    (newPos, newCol) = chompUpper src pos end col
  in
  if pos == newPos then
    (pos, pos, col)

  else if isDot src newPos end then
    foreignUpperHelp src (newPos + 1) end (newCol + 1)

  else
    (pos, newPos, newCol)



-- FOREIGN ALPHA


foreignAlpha : (P.Row -> P.Col -> x) -> P.Parser x Src.Expr_
foreignAlpha toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let ((alphaStart, alphaEnd), (newCol, varType)) = foreignAlphaHelp src pos end col in
    if alphaStart == alphaEnd then
      P.Eerr row newCol toError
    else
      let
        newState = P.State src alphaEnd end indent row newCol
        name = Name.fromPtr src alphaStart alphaEnd
      in
      if alphaStart == pos then
        if Set.member name reservedWords then
          P.Eerr row col toError
        else
          P.Cok (Src.Var varType name) newState
      else
        let home = Name.fromPtr src pos (alphaStart - 1) in
        P.Cok (Src.VarQual varType home name) newState


foreignAlphaHelp : String -> Int -> Int -> P.Col -> ((Int, Int), (P.Col, Src.VarType))
foreignAlphaHelp src pos end col =
  let
    (lowerPos, lowerCol) = chompLower src pos end col
  in
  if pos < lowerPos then
    ((pos, lowerPos), (lowerCol, Src.LowVar))

  else
    let
      (upperPos, upperCol) = chompUpper src pos end col
    in
    if pos == upperPos then
      ((pos, pos), (col, Src.CapVar))

    else if isDot src upperPos end then
      foreignAlphaHelp src (upperPos + 1) end (upperCol + 1)

    else
      ((pos, upperPos), (upperCol, Src.CapVar))



---- CHAR CHOMPERS ----



-- DOTS


isDot : String -> Int -> Int -> Bool
isDot src pos end =
  pos < end && P.unsafeIndex src pos == 0x2e {- . -}



-- UPPER CHARS


chompUpper : String -> Int -> Int -> P.Col -> (Int, P.Col)
chompUpper src pos end col =
  let width = getUpperWidth src pos end in
  if width == 0 then
    (pos, col)
  else
    chompInnerChars src (pos + width) end (col + 1)


getUpperWidth : String -> Int -> Int -> Int
getUpperWidth src pos end =
  if pos < end then
    getUpperWidthHelp (P.unsafeIndex src pos)
  else
    0


getUpperWidthHelp : Int -> Int
getUpperWidthHelp word =
  if 0x41 {- A -} <= word && word <= 0x5A {- Z -} then 1
  else if word < 0x80 then 0
  else if UChar.isUpper (Char.fromCode word) then P.getCharWidth word
  else 0


isUpper : Char -> Bool
isUpper char =
  getUpperWidthHelp (Char.toCode char) > 0



-- LOWER CHARS


chompLower : String -> Int -> Int -> P.Col -> (Int, P.Col)
chompLower src pos end col =
  let width = getLowerWidth src pos end in
  if width == 0 then
    (pos, col)
  else
    chompInnerChars src (pos + width) end (col + 1)


getLowerWidth : String -> Int -> Int -> Int
getLowerWidth src pos end =
  if pos < end then
    getLowerWidthHelp (P.unsafeIndex src pos)
  else
    0


getLowerWidthHelp : Int -> Int
getLowerWidthHelp word =
  if 0x61 {- a -} <= word && word <= 0x7A {- z -} then 1
  else if word < 0x80 then 0
  else if UChar.isLower (Char.fromCode word) then P.getCharWidth word
  else 0


isLower : Char -> Bool
isLower char =
  getLowerWidthHelp (Char.toCode char) > 0



-- INNER CHARS


chompInnerChars : String -> Int -> Int -> P.Col -> (Int, P.Col)
chompInnerChars src pos end col =
  let width = getInnerWidth src pos end in
  if width == 0 then
    (pos, col)
  else
    chompInnerChars src (pos + width) end (col + 1)


getInnerWidth : String -> Int -> Int -> Int
getInnerWidth src pos end =
  if pos < end then
    getInnerWidthHelp (P.unsafeIndex src pos)
  else
    0


getInnerWidthHelp : Int -> Int
getInnerWidthHelp word =
  if 0x61 {- a -} <= word && word <= 0x7A {- z -} then 1
  else if 0x41 {- A -} <= word && word <= 0x5A {- Z -} then 1
  else if 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} then 1
  else if word == 0x5F {- _ -} then 1
  else if word < 0x80 then 0
  else if UChar.isAlpha (Char.fromCode word) then P.getCharWidth word
  else 0


isInner : Char -> Bool
isInner char =
  getInnerWidthHelp (Char.toCode char) > 0
