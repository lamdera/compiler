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

import qualified AST.Literal as Lit
import AST.Expression.Optimized as Opt
import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg

import qualified Language.Haskell.Exts.Simple as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HPretty
import qualified Debug.Trace as Debug
import qualified Language.Haskell.Exts.Simple.Syntax as HSyntax

import Data.Ratio
import Data.List (intercalate)

(&) = flip ($)

-- GENERATE JAVASCRIPT


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName filepath info) =
  let
    genBody =
      do  defsList <- mapM JS.generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    genHsBody =
      do  defsList <- mapM generateDef (Module.program info)
          let managerStmts = generateEffectManager moduleName (Module.effects info)
          return (concat (defsList ++ [managerStmts]))

    jsBody =
      State.evalState genBody 0

    hsBody =
      genHsBody moduleName info
  in
    Debug.trace (show (Builder.stmtsToText jsBody))
      (Debug.trace (HPretty.prettyPrint hsBody)
        (Builder.stmtsToText jsBody))


-- GENERATE HASKELL

generateDef :: Opt.Def -> Hs.Decl
generateDef def =
  do  (home, name, jsBody) <-
          case def of
            Opt.TailDef (Opt.Facts home) name argNames body ->
                ("notimpl", "notimpl", "notimpl") -- (,,) home name <$> generateTailFunction name argNames body

            Opt.Def (Opt.Facts home) name body ->
                (,,) home name <$> tExpr body

      return (Var.define home name jsBody)




-- Maybe String because fn name is a Opt.Def in Elm, but an Expr in Haskell; we store the fn name here if we have one
tExpr :: Opt.Expr -> Hs.Exp
tExpr optExpr =
    Debug.trace ("generateJsExpr" ++ show optExpr) $
    case optExpr of
      Var var ->
          Hs.Var (tCanonVar var)

      Literal lit ->
          tLiteral lit

      Access record field ->
        let
          get =
            Hs.Var (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Ident "get"))
          fieldAst =
            Hs.OverloadedLabel field
          recordAst =
            tExpr record
        in
          Hs.App (Hs.App get fieldAst) recordAst

      Update record fields ->
        let
          set =
            Hs.Var (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Ident "set_"))
          recordAst =
            tExpr record
          fieldsAst =
            fields &
            fmap (\(field, expr) ->
              Hs.InfixApp
              (Hs.OverloadedLabel field)
              (Hs.QConOp (Hs.UnQual (Hs.Symbol ":=")))
              (tExpr expr)
            ) &
            foldl (\assignment state ->
              Hs.InfixApp
                state
                (Hs.QVarOp (Hs.UnQual (Hs.Symbol "&")))
                assignment
            )
            (Hs.Var (Hs.UnQual (Hs.Ident "rnil")))

        in
          Hs.App (Hs.App set recordAst) fieldsAst

      Record fields ->
        fields &
          fmap (\(field, expr) ->
            Hs.InfixApp
            (Hs.OverloadedLabel field)
            (Hs.QConOp (Hs.UnQual (Hs.Symbol ":=")))
            (tExpr expr)
          ) &
          foldl (\assignment state ->
            Hs.InfixApp
              state
              (Hs.QVarOp (Hs.UnQual (Hs.Symbol "&")))
              assignment
          )
          (Hs.Var (Hs.UnQual (Hs.Ident "rnil")))

      Binop op leftExpr rightExpr ->
        let
          larg = tExpr leftExpr
          rarg = tExpr rightExpr
          infixOp = tCanonVar op
        in
        Hs.InfixApp larg (Hs.QVarOp infixOp) rarg

      Function args body ->
        undefined -- this is handled at the Decl level, because haskell ast defines functions at that level, unlike the elm ast.
        -- TODO: Hs.UnGuardedRhs & Hs.Match (Hs.Ident (unwrap fnName)) (fmap (Hs.PVar . Hs.Ident) args) (tExpr body) Nothing

      Call func args ->
        undefined {-
          generateCall func args
  -}
      TailCall name argNames args ->
        undefined {-
          let
            reassign name tempName =
              ExprStmt () (AssignExpr () OpAssign (LVar () (Var.safe name)) (ref tempName))
          in
            do  args' <- mapM generateJsExpr args
                tempNames <- mapM (\_ -> Var.fresh) args
                jsBlock $
                  VarDeclStmt () (zipWith varDecl tempNames args')
                  : zipWith reassign argNames tempNames
                  ++ [ContinueStmt () (Just (Id () (Var.safe name)))]
  -}
      Let defs body ->
        undefined {-
          do  stmts <- mapM generateDef defs
              code <- generateCode body
              jsBlock (concat stmts ++ toStatementList code)
  -}
      If branches finally ->
        undefined {-
          generateIf branches finally
  -}
      Case exprName decider jumps ->
        undefined {-
          JsBlock <$> generateCase exprName decider jumps
  -}
      ExplicitList elements ->
        undefined {-
          do  jsElements <- mapM generateJsExpr elements
              jsExpr $ BuiltIn.list jsElements
  -}
      Data tag members ->
        undefined {-
        let
          ctor =
            "ctor" ==> StringLit () tag

          toEntry entry n =
            ("_" ++ show n) ==> entry
        in
          do  jsMembers <- mapM generateJsExpr members
              jsExpr $ ObjectLit () (ctor : zipWith toEntry jsMembers [ 0 :: Int .. ])
  -}
      DataAccess dataExpr index ->
        undefined {-
          do  jsDataExpr <- generateJsExpr dataExpr
              jsExpr $ DotRef () jsDataExpr (var ("_" ++ show index))
  -}
      Cmd moduleName ->
        undefined {-
          jsExpr $ BuiltIn.effect moduleName
  -}
      Sub moduleName ->
        undefined {-
          jsExpr $ BuiltIn.effect moduleName
  -}
      OutgoingPort name tipe ->
        undefined {-
          jsExpr $ BuiltIn.outgoingPort name (Foreign.encode tipe)
  -}
      IncomingPort name tipe ->
        undefined {-
          do  jsDecoder <- generateJsExpr (Foreign.decode tipe)
              jsExpr $ BuiltIn.incomingPort name jsDecoder
  -}
      Program kind body ->
        undefined {-
          generateProgram kind body
  -}
      GLShader _uid src _tipe ->
        undefined {-
          jsExpr $ ObjectLit () [(PropString () "src", Literal.literal (L.Str src))]
  -}
      Crash home region maybeBranchProblem ->
        undefined {-
          do  maybeOptBranchProblem <- traverse generateJsExpr maybeBranchProblem
              jsExpr $ BuiltIn.crash home region maybeOptBranchProblem
  -}


tLiteral :: Lit.Literal -> Hs.Exp
tLiteral lit =
  case lit of
    Lit.Chr char ->
      Hs.Lit (Hs.Char char)

    Lit.Str string ->
      Hs.Lit (Hs.String string)

    Lit.IntNum number ->
      Hs.Lit (Hs.Int (toInteger number))

    Lit.FloatNum number ->
      Hs.Lit (Hs.Frac (toRational number))

    Lit.Boolean boolean ->
      Hs.Con (Hs.UnQual (Hs.Ident (if boolean then "True" else "False")))

-- | Transpile a canonical variable
tCanonVar :: Var.Canonical -> Hs.QName
tCanonVar (Var.Canonical home name) =
  if Help.isOp name then
    undefined -- JS.BracketRef () (JS.ref (getOpsDictName home)) (JS.StringLit () name)

  else
    case home of
      Var.Local ->
        Hs.UnQual (Hs.Ident name)

      Var.BuiltIn ->
        undefined -- JS.ref (unqualified name)

      Var.Module moduleName@(ModuleName.Canonical _ rawName) ->
        if ModuleName.isNative rawName then
          undefined -- native moduleName name

        else
          Hs.Qual (Hs.ModuleName (intercalate "." rawName)) (Hs.Ident name)

      Var.TopLevel moduleName ->
        Hs.UnQual (Hs.Ident name)

-- HELPERS

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "tried to unwrap Nothing"


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
