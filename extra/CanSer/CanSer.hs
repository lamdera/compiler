{-# LANGUAGE OverloadedStrings #-}
module CanSer.CanSer (ppElm) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified AST.Canonical as C
import qualified Elm.Name as N
import qualified Data.Map as Map
import qualified Reporting.Annotation as A
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop
import Data.String (IsString)
import GHC.Exts(IsString(..))

{-|
Canonical ast serialization.

This is an attempt at serializing the canoncal ast back into elm code, so we can figure out what ast's represent more easily.

1. generate one-line elm code with semi-colons for indentation-sensitive line breaks
2. replace semicolons with indentation-sensitive line-breaks. Possibly ";+" and ";-" to increase/decrease indentation by 1
  - ;0 resets indentation

-}

ppElm :: ToElm a => a -> Text
ppElm a = pprint 0 (toList (toElm a))

pprint :: Int -> [Block] -> Text
pprint i (Lit s:rest) = T.replace "\n" ("\n" <> rep i "  ") s <> pprint i rest
pprint i (Indent:rest) = pprint (i+1) (Lit "\n" : rest)
pprint i (Dedent:rest) = pprint (i-1) (Lit "\n" : rest)
pprint _ (Join _ _:_) = error "unexpected Join when printing"
pprint _ [] = ""

rep i _ | i <= 0 = ""
rep i s = rep (i-1) s <> s

intercalate _ [] = Lit ""
intercalate _ [a] = a
intercalate sep (a:rest) = Join (Join a sep) (intercalate sep rest)

toList (Join a b) = toList a <> toList b
toList a = [a]

data Block
  = Lit Text
  | Join Block Block
  | Indent
  | Dedent
  deriving (Show)

instance Semigroup Block where
  (<>) = Join

instance IsString Block where
    fromString s = Lit (T.pack s)


class ToElm a where
  toElm :: a -> Block

infixr 6 <.>
a <.> b = a <> "." <> b

sp x = " " <> x <> " "
p x = "(" <> x <> ")"

instance ToElm N.Name where
  toElm x = Lit $ N.toText x

instance ToElm Double where
  toElm x = Lit $ T.pack $ show x


instance ToElm Int where
  toElm x = Lit $ T.pack $ show x

instance ToElm ModuleName.Canonical where
  toElm (ModuleName.Canonical _ modu) = toElm modu


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
      C.VarForeign moduName name _ ->
        toElm moduName <.> toElm name
      C.VarCtor _ moduName name _ _ ->
        toElm moduName <.> toElm name
      C.VarDebug moduName name _ ->
        toElm moduName <.> toElm name
      C.VarOperator op _ _ _ ->
        "(" <> toElm op <> ")"
      C.Chr text ->
        "'" <> Lit text <> "'"
      C.Str text ->
        "\"" <> Lit text <> "\""
      C.Int i ->
        toElm i
      C.Float d ->
        toElm d
      C.List [] ->
        "[]"
      C.List exprs ->
        "[" <> (intercalate ",\n" (toElm <$> exprs)) <> "\n]"
      C.Negate expr ->
        "-" <> toElm expr
      C.Binop op _ _ _ leftExpr rightExpr ->
        toElm leftExpr <> sp (toElm op) <> toElm rightExpr
      C.Lambda pats expr ->
        "(\\" <> (intercalate " " (toElm <$> pats)) <> " -> " <> (toElm expr) <> ")"
      C.Call expr argExprs ->
        p (toElm expr <> Indent <> (leftPad "\n" (toElm <$> argExprs)) <> Dedent)
      C.If thisThenExprs expr ->
        let
          f (pred, action) = "if " <> toElm pred <> "\nthen " <> toElm action <> "\nelse "
        in
          p (intercalate "" (f <$> thisThenExprs) <> toElm expr)
      C.Let def expr ->
        "let" <> Indent <> toElm def <> Dedent <> "in\n" <> Indent <> toElm expr <> Dedent
      C.LetRec defs expr ->
        "let" <> Indent <> (intercalate "\n" (toElm <$> defs)) <> Dedent <> "\nin " <> Indent <> toElm expr <> Dedent
      C.LetDestruct pat expr exprBody ->
        "let" <> Indent <> toElm pat <> " = " <> Indent <> toElm expr <> Dedent <> Dedent <> "\nin " <> Indent <> toElm exprBody <> Dedent
      C.Case expr caseBranches ->
        let
          f (C.CaseBranch pat expr1) = toElm pat <> " ->" <> Indent <> toElm expr1 <> Dedent
        in
        "case " <> toElm expr <> " of" <> Indent <> intercalate "\n" (f <$> caseBranches) <> Dedent
      C.Accessor name ->
        "." <> toElm name
      C.Access expr (A.At _ name) ->
        p(p (toElm expr) <.> toElm name)
      C.Update name _ nameFieldUpdateMap -> -- TODO: what is this `expr` for? When people try to record update expressions, rather than variables?
        "{ " <> toElm name <> " | " <> intercalate ", "
          ((\(name, value) -> toElm name <> " = " <> toElm value) <$> Map.toList (tFieldUpdate <$> nameFieldUpdateMap)) <> " }"
      C.Record nameExprMap ->
        "{ " <> intercalate ", " ((\(name, value) -> toElm name <> " = " <> toElm value) <$> Map.toList nameExprMap) <> " }"
      C.Unit ->
        "()"
      C.Tuple e1 e2 Nothing -> p (intercalate ", " (toElm <$> [e1, e2]))
      C.Tuple e1 e2 (Just e3) -> p (intercalate ", " (toElm <$> [e1, e2, e3]))
      C.Shader uid src _ ->
        "Shader " <> Lit uid <> " " <> Lit src <> " ??? "


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
      C.PCons p1 p2 -> toElm p1 <> " :: " <> toElm p2
      C.PBool _ bool -> if bool then "True" else "False"
      C.PChr text -> "'" <> Lit text <> "'"
      C.PStr text -> "\"" <> Lit text <> "\""
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

patternCtorArg (C.PatternCtorArg _ _ argPattern) = argPattern -- TODO: does index matter here? Is the list always sorted as we expect?



-- DEFINITIONS

instance ToElm C.Def where
  toElm def =
    case def of
      C.Def name pats expr ->
        toElm name <> leftPad " " (toElm <$> pats) <> " =" <> Indent <> toElm expr <> Dedent
      C.TypedDef name _ patTypes expr tipe ->
        toElm name <> " : " <> intercalate " -> " (toElm <$> ((snd <$> patTypes) <> [tipe])) <> "\n" <>
        toElm name <> leftPad " " (toElm <$> (fst <$> patTypes)) <> " =" <> Indent <> toElm expr <> Dedent


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
      C.TAlias moduName name _ _ -> toElm moduName <.> toElm name -- TODO: unsure if nameTypes should be serialized here


ftMap x =
  (\(f, t) -> toElm f <> " : " <> toElm (fieldType t)) <$> (Map.toList x)
fieldType (C.FieldType _ t) = t


leftPad :: Block -> [Block] -> Block
leftPad _ [] = ""
leftPad s xs = s <> intercalate s xs



-- MODULE

instance ToElm C.Module where
  toElm (C.Module name _ exports decls unions aliases binops _) =
    -- { _name    :: ModuleName.Canonical
    -- , _docs    :: Docs
    -- , _exports :: Exports
    -- , _decls   :: Decls
    -- , _unions  :: Map.Map N.Name Union
    -- , _aliases :: Map.Map N.Name Alias
    -- , _binops  :: Map.Map N.Name Binop
    -- , _effects :: Effects
    -- }
    "module " <> toElm name <> " exposing " <> toElm exports <> "\n"
      <> "-- NOTE: effects and docs are not serialized\n"
      <> intercalate "\n" ((\(name, union) -> "type " <> toElm name <> toElm union <> "\n") <$> (Map.toList unions)) <> "\n"
      <> intercalate "\n" ((\(name, alias) -> "type alias " <> toElm name <> toElm alias <> "\n") <$> (Map.toList aliases)) <> "\n"
      <> intercalate "\n" ((\(op, (C.Binop_ assoc precedence name)) ->
        "infix " <> toElm assoc <> " " <> toElm precedence <> " " <> p (toElm op) <> " = " <> toElm name <> "\n") <$> (Map.toList binops)) <> "\n"
      <> toElm decls <> "\n"

instance ToElm C.Decls where
  toElm decls =
    case decls of
      C.Declare def decls ->
        toElm def <> "\n" <> toElm decls
      C.DeclareRec defs decls ->
        intercalate "\n" (toElm <$> defs) <> toElm decls
      C.SaveTheEnvironment ->
        "-- SaveTheEnvironment"

-- EXPORTS

instance ToElm C.Exports where
  toElm exports =
    let
      f (name, C.ExportBinop) = p (toElm name)
      f (name, C.ExportUnionOpen) = p (toElm name) <> "(..)"
      f (name, _) = toElm name
    in
    case exports of
      C.ExportEverything _ -> "(..)"
      C.Export nameToExportMap -> "(" <> intercalate ", " (f <$> Map.toList ((\(A.At _ a) -> a) <$> nameToExportMap)) <> ")"

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
      C.Ctor name _ _ types ->
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











