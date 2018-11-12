{-# LANGUAGE OverloadedStrings #-}
module CanSer.CanSer where

import qualified Data.Text as T
import Data.Text (Text, intercalate)
import qualified AST.Canonical as C
import qualified Elm.Name as N
import Control.Monad.State.Lazy
import Data.Semigroup
import qualified Data.Map as Map
import qualified Reporting.Annotation as A
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop

{-|
Canonical ast serialization.

This is an attempt at serializing the canoncal ast back into elm code, so we can figure out what ast's represent more easily.

1. generate one-line elm code with semi-colons for indentation-sensitive line breaks
2. replace semicolons with indentation-sensitive line-breaks. Possibly ";+" and ";-" to increase/decrease indentation by 1
  - ;0 resets indentation

-}

ppElm a = T.pack (pprint 0 (T.unpack (toElm a)))

pprint i (';':'+':s) = "\n" <> rep (i+1) "  " <> pprint (i+1) s
pprint i (';':'-':s) = "\n" <> rep (i-1) "  " <> pprint (i-1) s
pprint i (';':'0':s) = "\n" <> pprint 0 s
pprint i (';':s) = "\n" <> rep i "  " <> pprint i s
pprint i (c:s) = c : pprint i s
pprint i [] = ""

rep i s | i < 0 = ""
rep i s = rep (i-1) s <> s

class ToElm a where
  toElm :: a -> Text

infixr 6 <.>
a <.> b = a <> "." <> b

sp x = " " <> x <> " "
p x = "(" <> x <> ")"

instance ToElm N.Name where
  toElm = N.toText

instance ToElm Double where
  toElm x = T.pack $ show x


instance ToElm Int where
  toElm x = T.pack $ show x

instance ToElm ModuleName.Canonical where
  toElm (ModuleName.Canonical pkg modu) = toElm modu


instance ToElm a => ToElm (A.Located a) where
  toElm (A.At _ a) = toElm a

-- EXPRESSION

instance ToElm C.Expr_ where
  toElm e =
    case e of
      C.VarLocal name ->
        toElm name
      C.VarTopLevel moduName name ->
        toElm moduName <.> toElm name
      C.VarKernel n1 n2 ->
        toElm n1 <.> toElm n2
      C.VarForeign moduName name annot ->
        toElm moduName <.> toElm name
      C.VarCtor ctorOpts moduName name zeroBasedIndex annot ->
        toElm moduName <.> toElm name
      C.VarDebug moduName name annot ->
        toElm moduName <.> toElm name
      C.VarOperator op moduName name annot ->
        toElm op
      C.Chr text ->
        "'" <> text <> "'"
      C.Str text ->
        "\"" <> text <> "\""
      C.Int i ->
        toElm i
      C.Float d ->
        toElm d
      C.List [] ->
        "[]"
      C.List exprs ->
        "[" <> (intercalate ",;" (toElm <$> exprs)) <> ";]"
      C.Negate expr ->
        "-" <> toElm expr
      C.Binop op _ _ annotation leftExpr rightExpr ->
        toElm leftExpr <> sp (toElm op) <> toElm rightExpr
      C.Lambda pats expr ->
        "(\\" <> (intercalate " " (toElm <$> pats)) <> " -> " <> (intercalate " " (toElm <$> pats)) <> ")"
      C.Call expr argExprs ->
        p (toElm expr <> (leftPad " " (toElm <$> argExprs)))
      C.If thisThenExprs expr ->
        let
          f (pred, action) = "if " <> toElm pred <> ";then " <> toElm action <> ";else "
        in
          p (intercalate "" (f <$> thisThenExprs) <> toElm expr)
      C.Let def expr ->
        "let " <> toElm def <> " in " <> toElm expr
      C.LetRec defs expr ->
        "let " <> (intercalate "; " (toElm <$> defs)) <> " in " <> toElm expr
      C.LetDestruct pat expr exprBody ->
        "let " <> toElm pat <> " = " <> toElm expr <> " in " <> toElm exprBody
      C.Case expr caseBranches ->
        let
          f (C.CaseBranch pat expr) = toElm pat <> " -> " <> toElm expr
        in
        "case " <> toElm expr <> " of;+" <> intercalate ";" (f <$> caseBranches) <> ";-"
      C.Accessor name ->
        "." <> toElm name
      C.Access expr (A.At _ name) ->
        p(p (toElm expr) <.> toElm name)
      C.Update name expr nameFieldUpdateMap -> -- TODO: what is this `expr` for? When people try to record update expressions, rather than variables?
        "{ " <> toElm name <> " | " <> intercalate ", "
          ((\(name, value) -> toElm name <> " = " <> toElm value) <$> Map.toList (tFieldUpdate <$> nameFieldUpdateMap)) <> " }"
      C.Record nameExprMap ->
        "{ " <> intercalate ", " ((\(name, value) -> toElm name <> " = " <> toElm value) <$> Map.toList nameExprMap) <> " }"
      C.Unit ->
        "()"
      C.Tuple e1 e2 Nothing -> p (intercalate ", " (toElm <$> [e1, e2]))
      C.Tuple e1 e2 (Just e3) -> p (intercalate ", " (toElm <$> [e1, e2, e3]))
      C.Shader uid src gltype ->
        "Shader " <> uid <> " " <> src <> " ??? "


tFieldUpdate (C.FieldUpdate _ e) = e


-- PATTERN

instance ToElm C.Pattern_ where
  toElm pattern =
    case pattern of
      C.PAnything -> "_"
      C.PVar name -> toElm name
      C.PRecord names -> "{ " <> intercalate ", " (toElm <$> names) <> " }"
      C.PAlias pat name -> p (toElm pat <> " as " <> toElm name)
      C.PUnit -> "()"
      C.PTuple p1 p2 Nothing -> p (intercalate ", " (toElm <$> [p1, p2]))
      C.PTuple p1 p2 (Just p3) -> p (intercalate ", " (toElm <$> [p1, p2, p3]))
      C.PList [] -> "[]"
      C.PList pats -> "[" <> (intercalate "," (toElm <$> pats)) <> "]"
      C.PCons p1 p2 -> toElm p1 <> " : " <> toElm p2
      C.PBool _ bool -> if bool then "True" else "False"
      C.PChr text -> "'" <> text <> "'"
      C.PStr text -> "\"" <> text <> "\""
      C.PInt i -> toElm i
      C.PCtor _home _type _union _name _index _args ->
        -- { _p_home :: ModuleName.Canonical
        -- , _p_type :: N.Name
        -- , _p_union :: Union
        -- , _p_name :: N.Name
        -- , _p_index :: Index.ZeroBased
        -- , _p_args :: [PatternCtorArg]
        -- }
        p (toElm _home <.> toElm _name <> leftPad " " (toElm <$> patternCtorArg <$> _args))

patternCtorArg (C.PatternCtorArg index tipe argPattern) = argPattern -- TODO: does index matter here? Is the list always sorted as we expect?



-- DEFINITIONS

instance ToElm C.Def where
  toElm def =
    case def of
      C.Def name pats expr ->
        toElm name <> leftPad " " (toElm <$> pats) <> " =;+ " <> toElm expr <> ";-"
      C.TypedDef name freeVars patTypes expr tipe ->
        toElm name <> " : " <> toElm tipe <> ";" <>
        toElm name <> leftPad " " (toElm <$> (fst <$> patTypes)) <> " =;+ " <> toElm expr <> ";-"


-- TYPES

instance ToElm C.Type where
  toElm tipe =
    case tipe of
      C.TLambda t1 t2 -> p (toElm t1 <> " -> " <> toElm t2)
      C.TVar name -> toElm name
      C.TType moduName name [] -> toElm moduName <.> toElm name
      C.TType moduName name types -> p (toElm moduName <.> toElm name <> leftPad " " (toElm <$> types))
      C.TRecord nameFieldTypeMap Nothing -> "{ " <> intercalate ", " (ftMap nameFieldTypeMap) <> " }"
      C.TRecord nameFieldTypeMap (Just name) -> "{ " <> toElm name <> " | " <> intercalate ", " (ftMap nameFieldTypeMap) <> " }"
      C.TUnit -> "()"
      C.TTuple t1 t2 Nothing -> p (intercalate ", " (toElm <$> [t1, t2]))
      C.TTuple t1 t2 (Just t3) -> p (intercalate ", " (toElm <$> [t1, t2, t3]))
      C.TAlias moduName name nameTypes aliasType -> toElm moduName <.> toElm name -- TODO: unsure if nameTypes should be serialized here


ftMap x =
  (\(f, t) -> toElm f <> " : " <> toElm (fieldType t)) <$> (Map.toList x)
fieldType (C.FieldType _ t) = t


leftPad s [] = ""
leftPad s xs = " " <> intercalate " " xs



-- MODULE

instance ToElm C.Module where
  toElm (C.Module name docs exports decls unions aliases binops effects) =
    -- { _name    :: ModuleName.Canonical
    -- , _docs    :: Docs
    -- , _exports :: Exports
    -- , _decls   :: Decls
    -- , _unions  :: Map.Map N.Name Union
    -- , _aliases :: Map.Map N.Name Alias
    -- , _binops  :: Map.Map N.Name Binop
    -- , _effects :: Effects
    -- }
    "module " <> toElm name <> " exposing " <> toElm exports <> ";"
      <> "-- NOTE: effects and docs are not serialized;"
      <> intercalate ";" ((\(name, union) -> "type " <> toElm name <> toElm union <> ";") <$> (Map.toList unions)) <> ";"
      <> intercalate ";" ((\(name, alias) -> "type alias " <> toElm name <> toElm alias <> ";") <$> (Map.toList aliases)) <> ";"
      <> intercalate ";" ((\(name, (C.Binop_ assoc precedence op)) ->
        "infix " <> toElm assoc <> " " <> toElm precedence <> " " <> p (toElm op) <> " = " <> toElm name <> ";") <$> (Map.toList binops)) <> ";"
      <> toElm decls <> ";"

instance ToElm C.Decls where
  toElm decls =
    case decls of
      C.Declare def decls ->
        toElm def <> ";" <> toElm decls
      C.DeclareRec defs decls ->
        intercalate ";" (toElm <$> defs) <> toElm decls
      C.SaveTheEnvironment ->
        "-- SaveTheEnvironment"

-- EXPORTS

instance ToElm C.Exports where
  toElm exports =
    case exports of
      C.ExportEverything _ -> "(..)"
      C.Export nameToExportMap -> "(" <> intercalate ", " ((\(name, exp) -> toElm name <> toElm exp) <$> Map.toList nameToExportMap) <> ")"

instance ToElm C.Export where
  toElm exp =
    case exp of
      C.ExportValue -> ""
      C.ExportBinop -> ""
      C.ExportAlias -> ""
      C.ExportUnionOpen -> "(..)"
      C.ExportUnionClosed -> ""
      C.ExportPort -> ""

-- UNION

instance ToElm C.Union where
  toElm exp =
    case exp of
      C.Union vars alts _ _ ->
      -- vars :: [N.Name]
      -- alts :: [Ctor]
        (leftPad " " (toElm <$> vars)) <> " = "
          <> intercalate " | " (toElm <$> alts)

instance ToElm C.Ctor where
  toElm ctor =
    case ctor of
      C.Ctor name indexZeroBased int types ->
        toElm name <> leftPad " " (toElm <$> types)


-- BINOP

instance ToElm Binop.Associativity where
  toElm assoc =
    case assoc of
      Binop.Left -> "left"
      Binop.Non -> "non"
      Binop.Right -> "right"

instance ToElm Binop.Precedence where
  toElm prec =
    case prec of
      Binop.Precedence i -> toElm i

-- ALIASES


instance ToElm C.Alias where
  toElm (C.Alias names tipe) =
    leftPad " " (toElm <$> names) <> " = " <> toElm tipe











