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
import qualified Transpile.Instances
import qualified Transpile.Imports
import qualified Transpile.Deriving

import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty

type List a = [a]

sShow a = pformat 0 $! show a -- T.unpack $! pShow a
tShow a = T.pack $ sShow a

pformat ident x =
  let
    ind = repeat ' ' & take (ident*2)
    indl = repeat ' ' & take ((ident-1)*2)
  in
  case x of
    '\n':rest -> "\n" ++ ind ++ pformat ident rest
    ',':rest -> "\n" ++ indl ++ "," ++ pformat ident rest
    '(':')':rest -> "()" ++ pformat ident rest
    '[':']':rest -> "[]" ++ pformat ident rest
    '(':rest -> "\n" ++ ind ++ "(" ++ pformat (ident+1) rest
    '[':rest -> "\n" ++ ind ++ "[" ++ pformat (ident+1) rest
    ')':rest -> ")\n" ++ indl ++ pformat (ident-1) rest
    ']':rest -> "]\n" ++ indl ++ pformat (ident-1) rest
    x:rest -> x : pformat ident rest
    [] -> ""


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
  ) annotations =
  let
    moduName = _tModuleName _name
    unions = _unions & tUnions
    unionTypeDecls = unions & fmap (Transpile.Instances.tDataToGADT moduName)
    instDecls = unions & concatMap (Transpile.Instances.tDataToInstDecl moduName)
  in
  let
    _decls' = concatMap (tDecls annotations) $ declsToList _decls
    v = unionTypeDecls <> instDecls <> _decls'
  in
  --DT.trace (sShow ("module", _name, _docs, _exports, _unions, _aliases, _binops, _effects)) $!
  DT.trace (T.unpack $ T.intercalate "\n\n" (fmap (\x -> {-tShow x <> "\n" <>-} T.pack (HsPretty.prettyPrint x)) v)) $!
  DT.trace (sShow ("annotations", annotations)) $!
   pure a

-- UNIONS

tUnions u =
  u & Map.toList & concatMap tUnion

tUnion (name, (C.Union tvars ctors _ _)) =
-- C.Ctor N.Name Index.ZeroBased _ [Type]
  let
    tCtor (C.Ctor name _ _ args) = Hs.ConDecl (ident name) (tType <$> args)
    dataCases = tCtor <$> ctors
  in
  [Hs.DataDecl
    Hs.DataType
    Nothing
    (foldl Hs.DHApp (Hs.DHead $ ident name) (Hs.UnkindedVar <$> ident <$> tvars))
    ((Hs.QualConDecl Nothing Nothing) <$> dataCases)
    []
  ]

-- DECLS

declsToList d = declsToList' 50 d

declsToList' n _ | n <= 0 = []
declsToList' n (C.Declare def decls) = [def] : declsToList' (n-1) decls
declsToList' n (C.DeclareRec defs decls) = defs : declsToList' (n-1) decls
declsToList' n (C.SaveTheEnvironment) = []

tDecls :: Map.Map N.Name C.Annotation -> List C.Def -> List Hs.Decl
tDecls annotations [def@(C.Def (A.At _ name) _ _)] = tAnnot name (Map.lookup name annotations) ++ tDef def
tDecls annotations defs = tRecDef defs

tAnnot :: N.Name -> Maybe C.Annotation -> [Hs.Decl]
tAnnot _ Nothing = []
tAnnot name (Just (C.Forall freeVars_ tipe)) =
  let
    freeVars = N.toText <$> Map.keys freeVars_
    hsType = tType tipe
  in
  [Hs.TypeSig [ident name] (Transpile.Deriving.addTypeConstraintsForType tipe $ tType tipe)]

tType t = case t of
  (C.TLambda t1 t2) -> Hs.TyFun (tType t1) (tType t2)
  (C.TVar name) | "number" `Text.isPrefixOf` N.toText name ->
    Hs.TyCon (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Ident "Double"))
  (C.TVar name) -> Hs.TyVar (ident name)
  (C.TType moduleName name types) -> Hs.TyCon (qual moduleName name) -- TODO: ?????? list constructor for cons mentions this type
  (C.TRecord mapNameToFieldType mName) ->
    let recordField (name, t) =
          Hs.TyInfix (Hs.TyPromoted (Hs.PromotedString (rawIdent name) (rawIdent name))) (Hs.PromotedName (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Symbol ":="))) (Hs.TyParen (tType $ t))
    in  Hs.TyParen $ Hs.TyApp (Hs.TyCon (Hs.Qual (Hs.ModuleName "Haskelm.Core") (Hs.Ident "Record'"))) (Hs.TyPromoted (Hs.PromotedList True (fmap recordField $ Map.toList $ fmap tFieldType $ mapNameToFieldType)))
  (C.TUnit) -> Hs.TyCon (Hs.Special (Hs.UnitCon))
  (C.TTuple t1 t2 Nothing) -> Hs.TyTuple Hs.Boxed [tType t1, tType t2]
  (C.TTuple t1 t2 (Just t3)) -> Hs.TyTuple Hs.Boxed [tType t1, tType t2, tType t3]
  (C.TAlias moduleName name nameTypePairs aliasType) -> error (sShow t)

tFieldType (C.FieldType w16 t) = t


tDef (C.Def (A.At _ name) pats e) =
  let
    (npats, ne) = Rewrite.recordArgsToLet pats e
  in
  DT.trace (sShow ("tDef", npats, ne, e)) $
  [Hs.FunBind [Hs.Match (ident name) (tPattern <$> npats) (Hs.UnGuardedRhs (tExpr ne)) Nothing]]
tDef td@(C.TypedDef name freeVars patTypeTuples e t) = tDef (C.Def name (fmap fst patTypeTuples) e) -- note: throwing away type information, relying on type inference to have picked it up for us

tRecDef a =
  concat $ tDef <$> a -- is this correct?

tExpr (A.At meta e) = case e of
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

  (C.Let def restExpr) -> Hs.Let (Hs.BDecls $ tDef def) (tExpr restExpr)
  (C.LetRec [] restExpr) -> tExpr restExpr -- for prettier output
  (C.LetRec defs restExpr) -> Hs.Let (Hs.BDecls $ concat $ tDef <$> defs) (tExpr restExpr)

  (C.LetDestruct pat e1 restExpr) ->
    let
      (np, ne1@(A.At meta (C.LetRec lrDefs lrExpr))) = Rewrite.recordArgToLet pat restExpr
      recNames = Rewrite.recordPatNames pat & snd & fmap fst
    in
    DT.trace (sShow ("letDest", "pat", pat, "e1", e1, "np", np, "restExpr", restExpr)) $
    DT.trace (sShow ("letDestLetRec", "pat", pat, "lrDefs", lrDefs, "lrExpr", lrExpr)) $
    Hs.Let (Hs.BDecls
      (((\recName -> Hs.FunBind [Hs.Match (ident recName) [] (Hs.UnGuardedRhs (tExpr e1)) Nothing]) <$> recNames)
      ++ (concat $ tDef <$> lrDefs)
      )) (tExpr restExpr)

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
qual moduleName name =
  Hs.Qual (Hs.ModuleName (_tModuleName moduleName)) (ident name)

_tModuleName (ModuleName.Canonical pkg modu) =
  Text.unpack ("Lamdera.Package." <> Pkg.toText pkg <> ".Module." <> N.toText modu)

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

ident name =
  let
    l = (rawIdent name)
  in
  Hs.Ident $
    if elem l Transpile.Imports.reservedWords then "l'" <> l else l

symIdent name =
  Hs.Symbol $
    case rawIdent name of
      "++"           -> "<>"
      "::"           -> ":"
      ":"            -> "::"
      (':':rest)     -> '+' : ':' : rest
      ('+':':':rest) -> '+' : ':' : ':' : rest
      a              -> a
rawIdent name = Text.unpack $ N.toText name

tFieldUpdate (C.FieldUpdate _ e) = e

tCtorArg (C.PatternCtorArg _ _ arg) = arg


tat (A.At _ v) = v

{-
letdestruct
  = let (x, y, z) = (1.0, 2.0, 3.0)
    in
      let
        rec'c =
          let
            c = (Lamdera.Haskelm.Core.get #c (rec'c))
          in
            (#a Lamdera.Haskelm.Core.:= (3.0)) Lamdera.Haskelm.Core.&
              (#b Lamdera.Haskelm.Core.:= (5.0)) Lamdera.Haskelm.Core.&
                (#c Lamdera.Haskelm.Core.:= (7.0)) Lamdera.Haskelm.Core.&
                  Lamdera.Haskelm.Core.rnil
      in
        let b = (Lamdera.Haskelm.Core.get #b (rec'b'a)) in
          let rec'b'a =
                  let a = (Lamdera.Haskelm.Core.get #a (rec'b'a)) in
                    (#a Lamdera.Haskelm.Core.:= (3.0)) Lamdera.Haskelm.Core.&
                      (#b Lamdera.Haskelm.Core.:= (5.0)) Lamdera.Haskelm.Core.&
                        Lamdera.Haskelm.Core.rnil
          in a + b


letdestruct =
  let
    (x,y,z) = (1,2,3)
    { a, b } =
      { a = 3, b = 5 }

    { c } =
      { a = 3, b = 5, c = 7 }
  in
  a + b

-}
