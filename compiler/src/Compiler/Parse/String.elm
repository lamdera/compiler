{- MANUALLY FORMATTED -}
module Compiler.Parse.String exposing
  ( string
  , character
  )


import Compiler.Data.Utf8 as Utf8
import Compiler.Elm.String as ES
import Compiler.Parse.Number as Number
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.List as MList exposing (TList)



-- CHARACTER


character : (P.Row -> P.Col -> x) -> (E.TChar -> P.Row -> P.Col -> x) -> P.Parser x ES.TString
character toExpectation toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos >= end || P.unsafeIndex src pos /= 0x27 {- ' -} then
      P.Eerr row col toExpectation

    else
      case chompChar src (pos + 1) end row (col + 1) 0 placeholder of
        Good newPos newCol numChars mostRecent ->
          if numChars /= 1 then
            P.Cerr row col (toError (E.CharNotString (newCol - col)))
          else
            let
              newState = P.State src newPos end indent row newCol
              char = ES.fromChunks src [mostRecent]
            in
            P.Cok char newState

        CharEndless newCol ->
          P.Cerr row newCol (toError E.CharEndless)

        CharEscape r c escape ->
          P.Cerr r c (toError (E.CharEscape escape))


type CharResult
  = Good Int P.Col Int ES.Chunk
  | CharEndless P.Col
  | CharEscape P.Row P.Col E.Escape


chompChar : String -> Int -> Int -> P.Row -> P.Col -> Int -> ES.Chunk -> CharResult
chompChar src pos end row col numChars mostRecent =
  if pos >= end then
    CharEndless col

  else
    let
      word = P.unsafeIndex src pos
    in
      if word == 0x27 {- ' -} then
        Good (pos + 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        CharEndless col

      else if word == 0x22 {- " -} then
        chompChar src (pos + 1) end row (col + 1) (numChars + 1) doubleQuote

      else if word == 0x5C {- \ -} then
        case eatEscape src (pos + 1) end row col of
          EscapeNormal ->
            chompChar src (pos + 2) end row (col + 2) (numChars + 1) (ES.Slice pos 2)

          EscapeUnicode delta code ->
            chompChar src (pos + delta) end row (col + delta) (numChars + 1) (ES.CodePoint code)

          EscapeProblem r c badEscape ->
            CharEscape r c badEscape

          EscapeEndOfFile ->
            CharEndless col

      else
        let
          width = P.getCharWidth word
          newPos = pos + width
        in
        chompChar src newPos end row (col + 1) (numChars + 1) (ES.Slice pos width)



-- STRINGS


string : (P.Row -> P.Col -> x) -> (E.TString -> P.Row -> P.Col -> x) -> P.Parser x ES.TString
string toExpectation toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    if isDoubleQuote src pos end then

      let
        pos1 = pos + 1
      in
      case
        if isDoubleQuote src pos1 end then
          let pos2 = pos + 2 in
          if isDoubleQuote src pos2 end then
            let
              pos3 = pos + 3
              col3 = col + 3
            in
            multiString src pos3 end row col3 pos3 row col []
          else
            COk pos2 row (col + 2) Utf8.empty
        else
          singleString src pos1 end row (col + 1) pos1 []
      of
        COk newPos newRow newCol utf8 ->
          let
            newState =
              P.State src newPos end indent newRow newCol
          in
          P.Cok utf8 newState

        CErr r c x ->
          P.Cerr r c (toError x)

    else
      P.Eerr row col toExpectation


isDoubleQuote : String -> Int -> Int -> Bool
isDoubleQuote src pos end =
  pos < end && P.unsafeIndex src pos == 0x22 {- " -}


type StringResult
  = COk Int P.Row P.Col ES.TString
  | CErr P.Row P.Col E.TString


finalize : String -> Int -> Int -> TList ES.Chunk -> ES.TString
finalize src start end revChunks =
  ES.fromChunks src <| MList.reverse <|
    if start == end then
      revChunks
    else
      ES.Slice start (end - start) :: revChunks


addEscape : ES.Chunk -> Int -> Int -> TList ES.Chunk -> TList ES.Chunk
addEscape chunk start end revChunks =
  if start == end then
    chunk :: revChunks
  else
    chunk :: ES.Slice start (end - start) :: revChunks



-- SINGLE STRINGS


singleString : String -> Int -> Int -> P.Row -> P.Col -> Int -> TList ES.Chunk -> StringResult
singleString src pos end row col initialPos revChunks =
  if pos >= end then
    CErr row col E.StringEndless_Single

  else
    let
      word = P.unsafeIndex src pos
    in
      if word == 0x22 {- " -} then
        COk (pos + 1) row (col + 1) <|
          finalize src initialPos pos revChunks

      else if word == 0x0A {- \n -} then
        CErr row col E.StringEndless_Single

      else if word == 0x27 {- ' -} then
        let newPos = pos + 1 in
        singleString src newPos end row (col + 1) newPos <|
          addEscape singleQuote initialPos pos revChunks

      else if word == 0x5C {- \ -} then
        case eatEscape src (pos + 1) end row col of
          EscapeNormal ->
            singleString src (pos + 2) end row (col + 2) initialPos revChunks

          EscapeUnicode delta code ->
            let newPos = pos + delta in
            singleString src newPos end row (col + delta) newPos <|
              addEscape (ES.CodePoint code) initialPos pos revChunks

          EscapeProblem r c x ->
            CErr r c (E.StringEscape x)

          EscapeEndOfFile ->
            CErr row (col + 1) E.StringEndless_Single

      else
        let newPos = pos + (P.getCharWidth word) in
        singleString src newPos end row (col + 1) initialPos revChunks



-- MULTI STRINGS


multiString : String -> Int -> Int -> P.Row -> P.Col -> Int -> P.Row -> P.Col -> TList ES.Chunk -> StringResult
multiString src pos end row col initialPos sr sc revChunks =
  if pos >= end then
    CErr sr sc E.StringEndless_Multi

  else
    let word = P.unsafeIndex src pos in
    if word == 0x22 {- " -} && isDoubleQuote src (pos + 1) end && isDoubleQuote src (pos + 2) end then
      COk (pos + 3) row (col + 3) <|
        finalize src initialPos pos revChunks

    else if word == 0x27 {- ' -} then
      let pos1 = pos + 1 in
      multiString src pos1 end row (col + 1) pos1 sr sc <|
        addEscape singleQuote initialPos pos revChunks

    else if word == 0x0A {- \n -} then
      let pos1 = pos + 1 in
      multiString src pos1 end (row + 1) 1 pos1 sr sc <|
        addEscape newline initialPos pos revChunks

    else if word == 0x0D {- \r -} then
      let pos1 = pos + 1 in
      multiString src pos1 end row col pos1 sr sc <|
        addEscape carriageReturn initialPos pos revChunks

    else if word == 0x5C {- \ -} then
      case eatEscape src (pos + 1) end row col of
        EscapeNormal ->
          multiString src (pos + 2) end row (col + 2) initialPos sr sc revChunks

        EscapeUnicode delta code ->
          let newPos = pos + delta in
          multiString src newPos end row (col + delta) newPos sr sc <|
            addEscape (ES.CodePoint code) initialPos pos revChunks

        EscapeProblem r c x ->
          CErr r c (E.StringEscape x)

        EscapeEndOfFile ->
          CErr sr sc E.StringEndless_Multi

    else
      let newPos = pos + (P.getCharWidth word) in
      multiString src newPos end row (col + 1) initialPos sr sc revChunks



-- ESCAPE CHARACTERS


type Escape
  = EscapeNormal
  | EscapeUnicode Int Int
  | EscapeEndOfFile
  | EscapeProblem P.Row P.Col E.Escape


eatEscape : String -> Int -> Int -> P.Row -> P.Col -> Escape
eatEscape src pos end row col =
  if pos >= end then
    EscapeEndOfFile

  else
    case P.unsafeIndex src pos of
      0x6E {- n -} -> EscapeNormal
      0x72 {- r -} -> EscapeNormal
      0x74 {- t -} -> EscapeNormal
      0x22 {- " -} -> EscapeNormal
      0x27 {- ' -} -> EscapeNormal
      0x5C {- \ -} -> EscapeNormal
      0x75 {- u -} -> eatUnicode src (pos + 1) end row col
      _            -> EscapeProblem row col E.EscapeUnknown


eatUnicode : String -> Int -> Int -> P.Row -> P.Col -> Escape
eatUnicode src pos end row col =
  if pos >= end || P.unsafeIndex src pos /= 0x7B {- { -} then
    EscapeProblem row col (E.BadUnicodeFormat 2)
  else
    let
      digitPos = pos + 1
      (newPos, code) = Number.chompHex src digitPos end
      numDigits = newPos - digitPos
    in
    if newPos >= end || P.unsafeIndex src newPos /= 0x7D {- } -} then
      EscapeProblem row col <| E.BadUnicodeFormat (2 + newPos - pos)

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem row col <| E.BadUnicodeCode (3 + newPos - pos)

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem row col <|
        E.BadUnicodeLength
          (3 + newPos - pos)
          numDigits
          code

    else
      EscapeUnicode (numDigits + 4) code


singleQuote : ES.Chunk
singleQuote =
  ES.Escape 0x27 {-'-}


doubleQuote : ES.Chunk
doubleQuote =
  ES.Escape 0x22 {-"-}


newline : ES.Chunk
newline =
  ES.Escape 0x6E {-n-}


carriageReturn : ES.Chunk
carriageReturn =
  ES.Escape 0x72 {-r-}


placeholder : ES.Chunk
placeholder =
  ES.CodePoint 0xFFFD {-replacement character-}
