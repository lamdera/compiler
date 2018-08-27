{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module East.Conversion where

import qualified Debug.Trace as DT
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as T
import qualified Data.Text as Text

import qualified AST.Canonical as C
import qualified East.V0_19 as E
import qualified Reporting.Annotation as A
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified AST.Module.Name as ModuleName
import Data.List (intercalate)

import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Monoid ((<>))

import qualified East.Rewrite as Rewrite

import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty

type List a = [a]

sShow a = T.unpack $ pShow a

-- transpile :: C.Module
transpile
  a@(C.Module
    _name    -- :: ModuleName.Canonical
    _docs    -- :: Docs
    _exports -- :: Exports
    _decls   -- :: Decls
    _unions  -- :: Map.Map N.Name Union
    _aliases -- :: Map.Map N.Name Alias
    _binops  -- :: Map.Map N.Name Binop
    _effects -- :: Effects
  ) =
  let
    v = concatMap tDecls $ declsToList _decls
  in
  DT.trace (intercalate "\n\n" (fmap (\x -> sShow x <> "\n" <> HsPretty.prettyPrint x) v)) $
   pure a

declsToList d = declsToList' 50 d
declsToList' n _ | n <= 0 = []
declsToList' n (C.Declare def decls) = [def] : declsToList' (n-1) decls
declsToList' n (C.DeclareRec defs decls) = defs : declsToList' (n-1) decls
declsToList' n (C.SaveTheEnvironment) = []

tDecls :: List C.Def -> List _
tDecls [def] = tDef def
tDecls defs = tRecDef defs

tDef (C.Def (A.At _ name) pats e) =
  let
    (npats, ne) = Rewrite.recordLets pats e
  in
  DT.trace (sShow ("tDef", npats, ne)) $
  [Hs.FunBind [Hs.Match (ident name) (tPattern <$> npats) (Hs.UnGuardedRhs (tExpr ne)) Nothing]]
tDef (C.TypedDef (A.At _ name) freeVars patTypeTuples e t) = [] -- TODO: impl

tRecDef a = error (sShow a)

--tLetDest (C.LetDestruct pat e1 restExpr) =
--  let
--    (np, ne1) = Rewrite.recordLet [pat] <$> e1
--  in C.Let (C.Def np [] ne1) $ tLetDest <$> restExpr
tLetDest a = a

tExpr expr@(A.At meta e) =
  tExpr' $ case e of
    (C.Lambda pats e1) -> e -- C.Lambda (Rewrite.recordPat <$> pats) (Rewrite.recordLet pats <$> e1) -- TODO: don't drop original exprs
    (C.LetDestruct pat e1 restExpr) -> tLetDest e
    (C.Case e1 branches) ->
      let
        newBranches = branches -- (\(C.CaseBranch pat expr) -> C.CaseBranch (Rewrite.recordPat pat) (Rewrite.recordLet [pat] <$> expr)) <$> branches -- TODO: rewrite pattern as well
      in
        C.Case e1 newBranches
    e -> e
    -- (C.Def) ->
    -- (C.TypedDef) ->

tExpr' e = case e of
  (C.VarLocal name) -> Hs.Var (Hs.UnQual $ ident name)
  (C.VarTopLevel moduleName name) -> Hs.Var (qual moduleName name)
  (C.VarKernel n1 n2) -> Hs.Var (Hs.Qual (Hs.ModuleName ("Lamdera.Haskelm.Kernel." ++ Text.unpack (N.toText n1))) (ident n2))
  (C.VarForeign moduleName name typeAnnotation) -> Hs.Var (qual moduleName name)
  (C.VarCtor _ moduleName name zeroBasedIndex typeAnnotation) -> Hs.Con (qual moduleName name)
  (C.VarDebug moduleName name typeAnnotation) -> error (sShow e)
  (C.VarOperator name1 moduleName name2 typeAnnotation) -> error (sShow e)
  (C.Chr text) -> Hs.Lit (Hs.String (Text.unpack text))
  (C.Str text) -> Hs.Lit (Hs.String (Text.unpack text))
  (C.Int int) -> Hs.Lit (Hs.Frac (toRational int))
  (C.Float double) -> Hs.Lit (Hs.Frac (toRational double))
  (C.List exprs) -> Hs.List (tExpr <$> exprs)
  (C.Negate e) -> Hs.NegApp (tExpr e)
  (C.Binop infixOp opModuleName textName typeAnnotation larg rarg) -> Hs.InfixApp (tExpr larg) (Hs.QVarOp $ Hs.UnQual $ symIdent infixOp) (tExpr rarg)
  (C.Lambda pats e) -> Hs.Lambda (tPattern <$> pats) (tExpr e)
  (C.Call expr exprs) -> foldl Hs.App (tExpr expr) (tExpr <$> exprs)
  (C.If exprPairs _else) ->
      let
        tIf [(cond, _then)] = Hs.If (tExpr cond) (tExpr _then) (tExpr _else)
        tIf ((cond, _then):rest) = Hs.If (tExpr cond) (tExpr _then) (tIf rest)
      in tIf exprPairs
  (C.Let def e1) -> Hs.Let (Hs.BDecls $ tDef def) (tExpr e1)
  (C.LetRec defs e1) -> Hs.Let (Hs.BDecls $ concat $ tDef <$> defs) (tExpr e1)
  (C.LetDestruct p e1 e2) -> error (sShow ("should be unreachable", e))
  (C.Case e caseBranches) ->
    Hs.Case (tExpr e) ((\(C.CaseBranch pat expr) -> Hs.Alt (tPattern pat) (Hs.UnGuardedRhs (tExpr expr)) Nothing) <$> caseBranches)
  (C.Accessor name) -> Hs.App (Hs.Var (Hs.UnQual (Hs.Ident "Lamdera.Haskelm.Core.get"))) (Hs.Var (Hs.UnQual (ident name)))
  (C.Access record (A.At _ fieldName)) ->
    let get       = Hs.Var (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "get"))
        fieldAst  = Hs.OverloadedLabel $ rawIdent fieldName
    in  Hs.Paren $ Hs.App (Hs.App get fieldAst) $ Hs.Paren (tExpr record)
  (C.Update base e nameFieldUpdateMap) ->
    let ast fieldName model value = Hs.App (Hs.App (Hs.App (Hs.Var (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Ident "set"))) (Hs.OverloadedLabel $ rawIdent fieldName)) (Hs.Paren $ tExpr value)) (Hs.Paren model)

        fieldsAst =
          nameFieldUpdateMap
            & Map.toList
            & reverse

        folder model (fieldName, fieldUpdate) = ast fieldName model (tFieldUpdate fieldUpdate)
    in Hs.Paren $ foldl folder (Hs.Var $ Hs.UnQual $ ident base) fieldsAst

  (C.Record fieldNameValueMap) ->
    -- E.Record Nothing fields trailingComments forceMultiline ->
    fieldNameValueMap
      & Map.toList
      & reverse
      & fmap (\(field, expr) -> Hs.InfixApp (Hs.OverloadedLabel $ rawIdent field) (Hs.QConOp (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Symbol ":="))) (Hs.Paren (tExpr expr)))
      & foldl (\assignment state -> Hs.InfixApp (Hs.Paren state) (Hs.QVarOp (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Symbol "&"))) assignment)
              (Hs.Var (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "rnil")))

  (C.Unit) -> Hs.Con (Hs.Special Hs.UnitCon)
  (C.Tuple e1 e2 Nothing) -> Hs.Tuple Hs.Boxed $ [tExpr e1, tExpr e2]
  (C.Tuple e1 e2 (Just e3)) -> Hs.Tuple Hs.Boxed $ [tExpr e1, tExpr e2, tExpr e3]
  (C.Shader text1 text2 _) -> error "shader not implemented on backend"

qual :: ModuleName.Canonical -> N.Name -> Hs.QName
qual (ModuleName.Canonical pkg modu) name =
  Hs.Qual (Hs.ModuleName (Text.unpack ("Lamdera.Package." <> Pkg.toText pkg <> ".Module." <> N.toText modu))) (ident name)


tPattern (A.At _ p) = case p of
  (C.PAnything) -> Hs.PWildCard
  (C.PUnit) -> Hs.PApp (Hs.Special Hs.UnitCon) []
  (C.PVar name) -> Hs.PVar (ident name)
  (C.PChr text) -> Hs.PLit Hs.Signless (Hs.String $ Text.unpack text) -- TODO: treating chars as strings
  (C.PStr text) -> Hs.PLit Hs.Signless (Hs.String $ Text.unpack text)
  (C.PInt int) -> Hs.PParen $ Hs.PLit Hs.Signless (Hs.Frac $ toRational int) -- TODO: signless?
  -- notimpl
  (C.PRecord names) -> error (sShow p)
  (C.PBool union bool) -> Hs.PApp (Hs.UnQual (Hs.Ident (if bool then "True" else "False"))) []
  -- recursive
  (C.PAlias pattern name) -> Hs.PAsPat (ident name) (tPattern pattern)
  (C.PTuple p1 p2 Nothing) -> Hs.PTuple Hs.Boxed $ [tPattern p1, tPattern p2]
  (C.PTuple p1 p2 (Just p3)) -> Hs.PTuple Hs.Boxed $ [tPattern p1, tPattern p2, tPattern p3]
  (C.PList pats) -> Hs.PList $ tPattern <$> pats
  (C.PCons p1 p2) ->
    Hs.PInfixApp (tPattern p1) (Hs.Special (Hs.Cons)) (tPattern p2)
  (C.PCtor
    _p_home_moduleName -- :: ModuleName.Canonical
    _p_type_name -- :: N.Name
    _p_union -- :: Union
    _p_constructor_name -- :: N.Name
    _p_index -- :: Index.ZeroBased
    _p_args -- :: [PatternCtorArg]
    ) -> --error (sShow p)
      Hs.PApp (qual _p_home_moduleName _p_constructor_name) (tPattern <$> tCtorArg <$> _p_args)

ident name = Hs.Ident (rawIdent name)
symIdent name = Hs.Symbol (rawIdent name)
rawIdent name = Text.unpack $ N.toText name

tFieldUpdate (C.FieldUpdate _ e) = e

tCtorArg (C.PatternCtorArg _ _ arg) = arg


tat (A.At _ v) = v
