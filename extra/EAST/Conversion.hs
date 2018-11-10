{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module East.Conversion where

import qualified Debug.Trace as DT
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as T
import qualified Data.Text as Text

import qualified AST.Canonical as C
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop

import qualified Reporting.Error as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning

import Data.List (intercalate)

import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified East.Rewrite as Rewrite
import qualified Transpile.Instances
import qualified Transpile.Reserved
import Transpile.Reserved (ident, symIdent, rawIdent)
import qualified Transpile.Deriving
import Transpile.PrettyPrint

import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty

type List a = [a]

{-
TODO: list of possible issues
- elm allows shadowing of imported things
  - import Bitwise exposing (or), then define `or` locally, the use `or`. Will fail in haskell due to ambiguity, but not in elm.
  - probably not an issue; references to top-level things seem to be fully qualified
-}

transpile
  :: C.Module
  -> Map.Map N.Name C.Annotation
  -> Map.Map N.Name ModuleName.Canonical
  -> Result.Result i [Warning.Warning] Error.Error Hs.Module
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
  ) annotations importDict =
  let
    moduName = _tModuleName _name
    unions = _unions & tUnions
    unionTypeDecls = unions & fmap (Transpile.Instances.tDataToGADT moduName)
    instDecls = unions & concatMap (Transpile.Instances.tDataToInstDecl moduName)

    aliasDecls = _aliases & Map.toList & concatMap tAlias

    _decls' = concatMap (tDecls annotations) $ declsToList _decls
    decls = unionTypeDecls <> aliasDecls <> instDecls <> _decls'

    imports = importDict & Map.toList & fmap tImport

    constructorFunctionsToExport = unions & concatMap (\v ->
      case v of
        (Hs.PatBind (Hs.PVar (Hs.Ident name)) _ _) | "constructor'" `List.isPrefixOf` name
          -> [name]
        _ -> []
      )

    -- binops = tBinops _binops
    moduleHead = Hs.ModuleHead (Hs.ModuleName moduName) Nothing (tExport _exports constructorFunctionsToExport)
    module_ = Hs.Module (Just moduleHead) [{-ModulePragma-}] (haskelmImports ++ imports) decls
  in
  -- DT.trace (sShow (moduName, imports, importDict)) $
  pure module_

-- IMPORTS

tImport (name, moduName) =
  Hs.ImportDecl
    (Hs.ModuleName $ _tModuleName moduName)
    True -- imported qualified?
    False -- imported with {-# SOURCE #-}?
    False -- import safe?
    Nothing -- imported with explicit package name
    (Just (Hs.ModuleName (rawIdent name))) -- optional alias name in an as clause.
    Nothing -- optional list of import specifications

haskelmImports =
  [ Hs.ImportDecl (Hs.ModuleName "Lamdera.Haskelm.Core") True False False Nothing Nothing Nothing
  , Hs.ImportDecl (Hs.ModuleName "Lamdera.Haskelm.Core") False False False Nothing Nothing
    (Just (Hs.ImportSpecList False (Hs.IVar <$> ((Hs.Ident <$> []) <> (Hs.Symbol <$> ["<>"])))))
  , Hs.ImportDecl (Hs.ModuleName "Basics") False False False Nothing Nothing Nothing
  ]

-- EXPORTS

tExport (C.ExportEverything region) _ = Nothing -- TODO: maybe walk over ast and explicitly export things that should be?
tExport (C.Export (nameExportMap)) constructorFunctionsToExport =
  nameExportMap
  & Map.toList
  & fmap (mapSnd tat)
  & concatMap tExportInner
  & (\v -> Hs.ExportSpecList (v ++ fmap (\c -> Hs.EVar (Hs.UnQual (Hs.Ident c))) constructorFunctionsToExport))
  & Just

tExportInner (name, C.ExportValue) = [Hs.EVar (Hs.UnQual (ident name))]
tExportInner (name, C.ExportBinop) = [Hs.EVar (Hs.UnQual (symIdent name))]
tExportInner (name, C.ExportAlias) = [Hs.EAbs (Hs.NoNamespace) (Hs.UnQual (ident name))
                                     -- type alias constructors are always exported
                                     ,Hs.EAbs (Hs.NoNamespace) (Hs.UnQual (ident $ toConstructorName name))
                                     ]
tExportInner (name, C.ExportUnionOpen) = [Hs.EThingWith (Hs.EWildcard 0) (Hs.UnQual (ident name)) []]
tExportInner (name, C.ExportUnionClosed) = [Hs.EThingWith (Hs.NoWildcard) (Hs.UnQual (ident name)) []]
tExportInner (name, C.ExportPort) = [error "ports are not available server-side"]

mapSnd fn (a,b) = (a, fn b)

-- BINOPS (not used, the list of allowed binops is hard-coded)
--tBinops b =
--  b & Map.toList & fmap tBinop
--
--tBinop (name, C.Binop_ associativity (Binop.Precedence precedence) n2) =
--  Hs.InfixDecl (tAssoc associativity) (Just precedence) [Hs.VarOp (symIdent name)]
--
--tAssoc (Binop.Left) = Hs.AssocLeft
--tAssoc (Binop.Non) = Hs.AssocNone
--tAssoc (Binop.Right) = Hs.AssocRight

-- UNIONS

tUnions u =
  u & Map.toList & concatMap tUnion

tUnion (name, (C.Union tvars ctors _ _)) =
-- C.Ctor N.Name Index.ZeroBased _ [Type]
  let
    tCtor (C.Ctor name _ _ args) = Hs.ConDecl (ident name) (tType <$> args)
    dataCases = tCtor <$> ctors
  in
  [ Hs.DataDecl
      Hs.DataType
      Nothing
      (foldl Hs.DHApp (Hs.DHead $ ident name) (Hs.UnkindedVar <$> ident <$> tvars))
      ((Hs.QualConDecl Nothing Nothing) <$> dataCases)
      [] -- deriving nothing
  ] ++ ((\(C.Ctor name _ _ _) -> Hs.PatBind (Hs.PVar (ident $ toConstructorName name)) (Hs.UnGuardedRhs (Hs.Con (Hs.UnQual (ident name)))) Nothing)
    <$> ctors)

-- TYPE ALIASES

tAlias (aliasName, C.Alias tvars t) =
  DT.trace (sShow ("tAlias", aliasName, "tvars", tvars, "t", t)) $
  [ Hs.TypeDecl
      (foldl Hs.DHApp (Hs.DHead $ ident aliasName) (Hs.UnkindedVar <$> ident <$> tvars))
      (tType t)
  ] ++ (
  case t of
    (C.TRecord mapNameToFieldType maybeExtendedRecordName) ->
      let
        fields = Map.toList $ fmap tFieldType $ mapNameToFieldType
      in
      [ Hs.FunBind
        [ Hs.Match
          (ident $ toConstructorName aliasName)
          (fields
           & fmap fst
           & fmap (\n -> Hs.PVar (ident n))
          )

          (Hs.UnGuardedRhs $
            fields
            & fmap fst
            & fmap (\fieldName -> Hs.InfixApp (Hs.OverloadedLabel $ rawIdent fieldName) (Hs.QConOp (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Symbol ":="))) (Hs.Paren (Hs.Var (Hs.UnQual (ident fieldName)))))
            & foldl (\assignment state -> Hs.InfixApp (Hs.Paren state) (Hs.QVarOp (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Symbol "&"))) assignment)
                    (Hs.Var (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "rnil"))))
          Nothing
        ]
      ]

    _ -> []
  )

-- DECLS

declsToList (C.Declare def decls) = [def] : declsToList decls
declsToList (C.DeclareRec defs decls) = defs : declsToList decls
declsToList (C.SaveTheEnvironment) = []

tDecls :: Map.Map N.Name C.Annotation -> List C.Def -> List Hs.Decl
tDecls annotations [def@(C.Def (A.At _ name) _ _)] = tAnnot name (Map.lookup name annotations) ++ tDef def
tDecls annotations defs = concat $ tDef <$> defs

tAnnot :: N.Name -> Maybe C.Annotation -> [Hs.Decl]
tAnnot _ Nothing = error "missing type annotation"
tAnnot name (Just (C.Forall freeVars_ tipe)) =
  let
    freeVars = N.toText <$> Map.keys freeVars_
    hsType = tType tipe
  in
  [Hs.TypeSig [ident name] (Transpile.Deriving.addTypeConstraintsForType tipe $ tType tipe)]

tType t = case t of
  (C.TVar name) | "number" `Text.isPrefixOf` N.toText name ->
    Hs.TyCon (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "Double"))
  (C.TVar name) ->
    Hs.TyVar (ident name)
  (C.TLambda t1 t2) -> Hs.TyFun (tType t1) (tType t2)
  (C.TType moduleName name types) -> foldl Hs.TyApp (Hs.TyCon (qual moduleName name)) (tType <$> types)
  (C.TRecord mapNameToFieldType maybeExtendedRecordName) ->
    let recordField (name, t) =
          Hs.TyInfix (Hs.TyPromoted (Hs.PromotedString (rawIdent name) (rawIdent name))) (Hs.UnpromotedName (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Symbol ":="))) (Hs.TyParen (tType $ t))
    in  Hs.TyParen $ Hs.TyApp (Hs.TyCon (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "Record'"))) (Hs.TyPromoted (Hs.PromotedList True (fmap recordField $ Map.toList $ fmap tFieldType $ mapNameToFieldType)))
  (C.TUnit) -> Hs.TyCon (Hs.Special (Hs.UnitCon))
  (C.TTuple t1 t2 Nothing) -> Hs.TyTuple Hs.Boxed [tType t1, tType t2]
  (C.TTuple t1 t2 (Just t3)) -> Hs.TyTuple Hs.Boxed [tType t1, tType t2, tType t3]
  (C.TAlias moduleName name nameTypePairs aliasType) ->
    -- aliasType = the "original" type we're aliasing, right hand side of type alias declaration
    --
    let
      conv t =
        foldl -- T a b instead of a b T
          Hs.TyApp
          (Hs.TyCon (qual moduleName name))
          (nameTypePairs & fmap snd & fmap tType)
    in
    case aliasType of
      C.Holey t ->
        conv t

      C.Filled t ->
        -- used e.g. in VirtualDom.Node, guessing this refers to whether the right hand side has any type variables or not.
        conv t


tFieldType (C.FieldType w16 t) = t


tDef (C.Def (A.At _ name) pats e) =
  let
    (npats, ne1) = Rewrite.recordArgsToLet pats e
  in
  [Hs.FunBind [Hs.Match (ident name) (tPattern <$> npats) (Hs.UnGuardedRhs (tExpr ne1)) Nothing]]
tDef td@(C.TypedDef name freeVars patTypeTuples e t) =
  tAnnot (tat name) (Just $ C.Forall freeVars
    ( ((patTypeTuples & fmap snd) ++ [t])
      & foldr1 C.TLambda -- (a -> b) -> c vs a -> b -> c
    )
  ) ++
  tDef (C.Def name (fmap fst patTypeTuples) e)

tExpr (A.At meta e) = case e of
  (C.VarLocal name) -> Hs.Var (Hs.UnQual $ ident name)
  (C.VarTopLevel moduleName name) -> Hs.Var (qual moduleName name)
  (C.VarKernel n1 n2) -> Hs.Var (Hs.Qual (Hs.ModuleName ("Lamdera.Haskelm.Kernel." ++ Text.unpack (N.toText n1))) (ident n2))
  (C.VarForeign moduleName name typeAnnotation) -> Hs.Var (qual moduleName name)
  (C.VarCtor _ moduleName name zeroBasedIndex typeAnnotation) ->
    Hs.Con (qual moduleName (toConstructorName name))
  (C.VarDebug moduleName name typeAnnotation) -> Hs.Var (qual moduleName name) -- error (sShow e) -- TODO: don't allow debug vars
  (C.VarOperator op moduleName symbolName typeAnnotation) -> Hs.Var (Hs.UnQual (symIdent op))
  (C.Chr text) -> Hs.Lit (Hs.String (Text.unpack text))
  (C.Str text) -> Hs.Lit (Hs.String (Text.unpack text))
  (C.Int int) -> Hs.Lit (Hs.Frac (toRational int))
  (C.Float double) -> Hs.Lit (Hs.Frac (toRational double))
  (C.List exprs) -> Hs.List (tExpr <$> exprs)
  (C.Negate e) -> Hs.NegApp (tExpr e)
  (C.Binop infixOp opModuleName textName typeAnnotation larg rarg) -> Hs.InfixApp (tExpr larg) (Hs.QVarOp $ Hs.UnQual $ symIdent infixOp) (tExpr rarg)
  (C.Lambda pats e) ->
    let
      (npats, ne) = Rewrite.recordArgsToLet pats e
    in
    Hs.Lambda (tPattern <$> npats) (tExpr ne)
  (C.Call expr exprs) -> foldl Hs.App (tExpr expr) (tExpr <$> exprs)
  (C.If exprPairs _else) ->
      let
        tIf [(cond, _then)] = Hs.If (tExpr cond) (tExpr _then) (tExpr _else)
        tIf ((cond, _then):rest) = Hs.If (tExpr cond) (tExpr _then) (tIf rest)
      in tIf exprPairs

  (C.Let def restExpr) -> Hs.Let (Hs.BDecls $ tDef def) (tExpr restExpr)
  (C.LetRec [] restExpr) -> tExpr restExpr -- for prettier output
  (C.LetRec defs restExpr) -> Hs.Let (Hs.BDecls $ concat $ tDef <$> defs) (tExpr restExpr)

  (C.LetDestruct pat e1 restExpr) ->
    let
      (np, ne1@(A.At meta (C.LetRec lrDefs lrExpr))) = Rewrite.recordArgToLet pat restExpr
      recNames = Rewrite.recordPatNames pat & snd & fmap fst
    in
    Hs.Let
      (Hs.BDecls
        ((Hs.PatBind (tPattern np) (Hs.UnGuardedRhs (tExpr e1)) Nothing)
        : (concat $ tDef <$> lrDefs)
        )
      )
      (tExpr lrExpr) -- TODO: or restExpr?

  (C.Case e caseBranches) ->
    let
      newCaseBranches = rewrite <$> caseBranches
      rewrite (C.CaseBranch pat expr) =
        let
          (npat, nexpr) = Rewrite.recordArgToLet pat expr
        in C.CaseBranch npat nexpr
    in
    Hs.Case (tExpr e) ((\(C.CaseBranch pat expr) -> Hs.Alt (tPattern pat) (Hs.UnGuardedRhs (tExpr expr)) Nothing) <$> newCaseBranches)
  (C.Accessor name) -> Hs.App (Hs.Var (Hs.UnQual (Hs.Ident "Lamdera.Haskelm.Core.get"))) (Hs.Var (Hs.UnQual (ident name)))
  (C.Access record (A.At _ fieldName)) ->
    let get       = Hs.Var (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "get"))
        fieldAst  = Hs.OverloadedLabel $ rawIdent fieldName
    in  Hs.Paren $ Hs.App (Hs.App get fieldAst) $ Hs.Paren (tExpr record)
  (C.Update base e nameFieldUpdateMap) ->
    let ast fieldName model value = Hs.App (Hs.App (Hs.App (Hs.Var (Hs.Qual (Hs.ModuleName "Lamdera.Haskelm.Core") (Hs.Ident "set"))) (Hs.OverloadedLabel $ rawIdent fieldName)) (Hs.Paren $ tExpr value)) (Hs.Paren model)

        fieldsAst =
          nameFieldUpdateMap
            & Map.toList
            & reverse

        folder model (fieldName, fieldUpdate) = ast fieldName model (tFieldUpdate fieldUpdate)
    in Hs.Paren $ foldl folder (Hs.Var $ Hs.UnQual $ ident base) fieldsAst

  (C.Record fieldNameValueMap) ->
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
qual moduleName name =
  Hs.Qual (Hs.ModuleName (_tModuleName moduleName)) (ident name)

_tModuleName (ModuleName.Canonical pkg modu) =
  let
    (author, project) = Pkg.unpack pkg
    capitalize t =
      t
      & Text.unpack
      & (\(x:xs) -> Char.toUpper x : xs)
      & Text.pack
  in
  -- Text.unpack ("Lamdera.UserCode.Author." <> capitalize author <> ".Project." <> capitalize project <> ".Module." <> N.toText modu)
  Text.unpack (N.toText modu)

tPattern (A.At _ p) = case p of
  (C.PAnything) -> Hs.PWildCard
  (C.PUnit) -> Hs.PApp (Hs.Special Hs.UnitCon) []
  (C.PVar name) -> Hs.PVar (ident name)
  (C.PChr text) -> Hs.PLit Hs.Signless (Hs.String $ Text.unpack text) -- TODO: treating chars as strings
  (C.PStr text) -> Hs.PLit Hs.Signless (Hs.String $ Text.unpack text)
  (C.PInt int) -> Hs.PParen $ Hs.PLit Hs.Signless (Hs.Frac $ toRational int) -- TODO: signless?
  (C.PRecord names) -> error (show "C.PRecord got through Rewrite: " ++ sShow p)
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


tFieldUpdate (C.FieldUpdate _ e) = e

tCtorArg (C.PatternCtorArg _ _ arg) = arg


tat (A.At _ v) = v
at = A.At R.zero

toConstructorName name = N.fromText ("constructor'" <> (N.toText name))

