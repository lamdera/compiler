module Generate.Haskell (generate) where

import qualified Control.Monad.State as State
import qualified Data.Text.Lazy as LazyText
import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Effects as Effects
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as Builder
import qualified Generate.JavaScript.Expression as JS
import qualified Generate.JavaScript.Helpers as JS
import qualified Generate.JavaScript.Variable as Var

--

import qualified Language.Haskell.Exts as HS
import qualified Language.Haskell.Exts.Pretty as HPretty
import qualified Debug.Trace as Debug
import qualified Language.Haskell.Exts.Syntax as HSyntax

-- GENERATE JAVASCRIPT


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    genBody =
      do  defsList <- mapM JS.generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    jsBody =
      State.evalState genBody 0

    hsBody =
      HSyntax.Ident () "test"
  in
    Debug.trace (show (Builder.stmtsToText jsBody))
      (Debug.trace (HPretty.prettyPrint hsBody)
        (Builder.stmtsToText jsBody))
-- TODO: next up, import haskell ast and stdout show it, along with the corresponding js


-- GENERATE EFFECT MANAGER


generateEffectManager :: ModuleName.Canonical -> Effects.Canonical -> [JS.Statement ()]
generateEffectManager moduleName effects =
  case effects of
    Effects.None ->
      []

    Effects.Port _ ->
      []

    Effects.Manager pkgName (Effects.Info _ _ _ _ managerType) ->
      let
        managers =
          Var.coreNative "Platform" "effectManagers"

        managerName =
          JS.StringLit () (ModuleName.canonicalToString moduleName)

        entry name =
          ( JS.PropId () (JS.Id () name)
          , JS.ref (Var.qualified moduleName name)
          )

        managerEntries =
          [ "pkg" ==> Pkg.toString pkgName
          , entry "init"
          , entry "onEffects"
          , entry "onSelfMsg"
          ]

        otherEntries =
          case managerType of
            Effects.CmdManager _ ->
              [ "tag" ==> "cmd", entry "cmdMap" ]

            Effects.SubManager _ ->
              [ "tag" ==> "sub", entry "subMap" ]

            Effects.FxManager _ _ ->
              [ "tag" ==> "fx", entry "cmdMap", entry "subMap" ]

        addManager =
          JS.AssignExpr () JS.OpAssign
            (JS.LBracket () managers managerName)
            (JS.ObjectLit () (managerEntries ++ otherEntries))
      in
        [ JS.ExprStmt () addManager ]


(==>) :: String -> String -> ( JS.Prop (), JS.Expression () )
(==>) key value =
  ( JS.PropId () (JS.Id () key)
  , JS.StringLit () value
  )
