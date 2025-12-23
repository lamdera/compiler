{- MANUALLY FORMATTED -}
module Compiler.Parse.Space exposing
  ( Parser
  --
  , chomp
  , chompAndCheckIndent
  --
  , checkIndent
  , checkAligned
  , checkFreshLine
  --
  , docComment
  )


import Compiler.AST.Source as Src
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- SPACE PARSING


type alias Parser x a =
  P.Parser x (a, A.Position)



-- CHOMP


chomp : (E.Space -> P.Row -> P.Col -> x) -> P.Parser x ()
chomp toError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      ((status, newPos), (newRow, newCol)) = eatSpaces src pos end row col
    in
    case status of
      Good ->
        let
          newState = P.State src newPos end indent newRow newCol
        in
        P.Cok () newState

      HasTab               -> P.Cerr newRow newCol (toError E.HasTab)
      EndlessMultiComment  -> P.Cerr newRow newCol (toError E.EndlessMultiComment)



-- CHECKS -- to be called right after a `chomp`


checkIndent : A.Position -> (P.Row -> P.Col -> x) -> P.Parser x ()
checkIndent (A.Position endRow endCol) toError =
  P.Parser <| \((P.State _ _ _ indent _ col) as state) ->
    if col > indent && col > 1
    then P.Eok () state
    else P.Eerr endRow endCol toError


checkAligned : (Int -> P.Row -> P.Col -> x) -> P.Parser x ()
checkAligned toError =
  P.Parser <| \((P.State _ _ _ indent row col) as state) ->
    if col == indent
    then P.Eok () state
    else P.Eerr row col (toError indent)


checkFreshLine : (P.Row -> P.Col -> x) -> P.Parser x ()
checkFreshLine toError =
  P.Parser <| \((P.State _ _ _ _ row col) as state) ->
    if col == 1
    then P.Eok () state
    else P.Eerr row col toError



-- CHOMP AND CHECK


chompAndCheckIndent : (E.Space -> P.Row -> P.Col -> x) -> (P.Row -> P.Col -> x) -> P.Parser x ()
chompAndCheckIndent toSpaceError toIndentError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      ((status, newPos), (newRow, newCol)) = eatSpaces src pos end row col
    in
    case status of
      Good ->
        if newCol > indent && newCol > 1
        then

          let
            newState = P.State src newPos end indent newRow newCol
          in
          P.Cok () newState

        else
          P.Cerr row col toIndentError

      HasTab               -> P.Cerr newRow newCol (toSpaceError E.HasTab)
      EndlessMultiComment  -> P.Cerr newRow newCol (toSpaceError E.EndlessMultiComment)



-- EAT SPACES


type Status
  = Good
  | HasTab
  | EndlessMultiComment


eatSpaces : String -> Int -> Int -> P.Row -> P.Col -> ((Status, Int), (P.Row, P.Col))
eatSpaces src pos end row col =
  if pos >= end then
    ((Good, pos), (row, col))

  else
    case P.unsafeIndex src pos of
      0x20 {-   -} ->
        eatSpaces src (pos + 1) end row (col + 1)

      0x0A {- \n -} ->
        eatSpaces src (pos + 1) end (row + 1) 1

      0x7B {- { -} ->
        let ((maybeStatus, newPos), (newRow, newCol)) = eatMultiComment src pos end row col in
        case maybeStatus of
          Nothing     -> eatSpaces src newPos end newRow newCol
          Just status -> ((status, newPos), (newRow, newCol))

      0x2D {- - -} ->
        let pos1 = pos + 1 in
        if pos1 < end && P.unsafeIndex src pos1 == 0x2D {- - -} then
          let ((maybeStatus, newPos), (newRow, newCol)) = eatLineComment src (pos + 2) end row (col + 2) in
          case maybeStatus of
            Nothing     -> eatSpaces src newPos end newRow newCol
            Just status -> ((status, newPos), (newRow, newCol))
        else
          ((Good, pos), (row, col))

      0x0D {- \r -} ->
        eatSpaces src (pos + 1) end row col

      0x09 {- \t -} ->
        ((HasTab, pos), (row, col))

      _ ->
        ((Good, pos), (row, col))



-- LINE COMMENTS


eatLineComment : String -> Int -> Int -> P.Row -> P.Col -> ((Maybe Status, Int), (P.Row, P.Col))
eatLineComment src pos end row col =
  if pos >= end then
    ((Just Good, pos), (row, col))

  else
    let word = P.unsafeIndex src pos in
    if word == 0x0A {- \n -} then
      ((Nothing, pos + 1), (row + 1, 1))
    else
      let newPos = pos + (P.getCharWidth word) in
      eatLineComment src newPos end row (col + 1)



-- MULTI COMMENTS


eatMultiComment : String -> Int -> Int -> P.Row -> P.Col -> ((Maybe Status, Int), (P.Row, P.Col))
eatMultiComment src pos end row col =
  let
    pos1 = pos + 1
    pos2 = pos + 2
  in
  if pos2 >= end then
    ((Just Good, pos), (row, col))

  else if P.unsafeIndex src pos1 == 0x2D {- - -} then

    if P.unsafeIndex src pos2 == 0x7C {- | -} then
      ((Just Good, pos), (row, col))
    else
      let
        ((status, newPos), (newRow, newCol)) =
          eatMultiCommentHelp src pos2 end row (col + 2) 1
      in
      case status of
        MultiGood    -> ((Nothing, newPos), (newRow, newCol))
        MultiTab     -> ((Just HasTab, newPos), (newRow, newCol))
        MultiEndless -> ((Just EndlessMultiComment, pos), (row, col))

  else
    ((Just Good, pos), (row, col))


type MultiStatus
  = MultiGood
  | MultiTab
  | MultiEndless


eatMultiCommentHelp : String -> Int -> Int -> P.Row -> P.Col -> Int -> ((MultiStatus, Int), (P.Row, P.Col))
eatMultiCommentHelp src pos end row col openComments =
  if pos >= end then
    ((MultiEndless, pos), (row, col))

  else
    let word = P.unsafeIndex src pos in
    if word == 0x0A {- \n -} then
      eatMultiCommentHelp src (pos + 1) end (row + 1) 1 openComments

    else if word == 0x09 {- \t -} then
      ((MultiTab, pos), (row, col))

    else if word == 0x2D {- - -} && P.isWord src (pos + 1) end 0x7D {- } -} then
      if openComments == 1 then
        ((MultiGood, pos + 2), (row, col + 2))
      else
        eatMultiCommentHelp src (pos + 2) end row (col + 2) (openComments - 1)

    else if word == 0x7B {- { -} && P.isWord src (pos + 1) end 0x2D {- - -} then
      eatMultiCommentHelp src (pos + 2) end row (col + 2) (openComments + 1)

    else
      let newPos = pos + (P.getCharWidth word) in
      eatMultiCommentHelp src newPos end row (col + 1) openComments



-- DOCUMENTATION COMMENT


docComment : (P.Row -> P.Col -> x) -> (E.Space -> P.Row -> P.Col -> x) -> P.Parser x Src.Comment
docComment toExpectation toSpaceError =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      pos3 = pos + 3
    in
    if pos3 <= end
      && P.unsafeIndex src (pos    ) == 0x7B {- { -}
      && P.unsafeIndex src (pos + 1) == 0x2D {- - -}
      && P.unsafeIndex src (pos + 2) == 0x7C {- | -}
    then
      let
        col3 = col + 3

        ((status, newPos), (newRow, newCol)) =
          eatMultiCommentHelp src pos3 end row col3 1
      in
      case status of
        MultiGood ->
          let
            comment = Src.Comment
            newState = P.State src newPos end indent newRow newCol
          in
          P.Cok comment newState

        MultiTab -> P.Cerr newRow newCol (toSpaceError E.HasTab)
        MultiEndless -> P.Cerr row col (toSpaceError E.EndlessMultiComment)
    else
      P.Eerr row col toExpectation
