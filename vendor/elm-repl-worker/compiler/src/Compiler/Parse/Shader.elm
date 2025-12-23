{- MANUALLY FORMATTED -}
module Compiler.Parse.Shader exposing
  ( shader
  )


import Compiler.AST.Source as Src
import Compiler.AST.Utils.Shader as Shader
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Extra.Type.Set as Set



-- SHADER


shader : A.Position -> P.Parser E.Expr Src.Expr
shader ((A.Position row col) as start) =
  P.bind parseBlock <| \block ->
  P.bind (parseGlsl row col block) <| \shdr ->
  P.bind P.getPosition <| \end ->
  P.return (A.at start end (Src.Shader (Shader.fromChars block) shdr))



-- BLOCK


parseBlock : P.Parser E.Expr String
parseBlock =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      pos6 = pos + 6
    in
    if pos6 <= end
      && P.unsafeIndex src (pos    ) == 0x5B {- [ -}
      && P.unsafeIndex src (pos + 1) == 0x67 {- g -}
      && P.unsafeIndex src (pos + 2) == 0x6C {- l -}
      && P.unsafeIndex src (pos + 3) == 0x73 {- s -}
      && P.unsafeIndex src (pos + 4) == 0x6C {- l -}
      && P.unsafeIndex src (pos + 5) == 0x7C {- | -}
    then
      let
        ((status, newPos), (newRow, newCol)) =
          eatShader src pos6 end row (col + 6)
      in
      case status of
        Good ->
          let
            off = pos6
            len = newPos - pos6
            block = String.slice off (off + len) src
            newState = P.State src (newPos + 2) end indent newRow (newCol + 2)
          in
          P.Cok block newState

        Unending ->
          P.Cerr row col E.EndlessShader

    else
      P.Eerr row col E.Start


type Status
  = Good
  | Unending


eatShader : String -> Int -> Int -> P.Row -> P.Col -> ((Status, Int), (P.Row, P.Col))
eatShader src pos end row col =
  if pos >= end then
    ((Unending, pos), (row, col))

  else
    let word = P.unsafeIndex src pos in
    if word == 0x007C {- | -} && P.isWord src (pos + 1) end 0x5D {- ] -} then
      ((Good, pos), (row, col))

    else if word == 0x0A {- \n -} then
      eatShader src (pos + 1) end (row + 1) 1

    else
      let newPos = pos + (P.getCharWidth word) in
      eatShader src newPos end row (col + 1)



-- GLSL


parseGlsl : P.Row -> P.Col -> String -> P.Parser E.Expr Shader.Types
parseGlsl _ _ _ =
  -- TODO: Parse.Shader.parseGlsl
  P.return <| Shader.Types Set.empty Set.empty Set.empty
