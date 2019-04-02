module East.Plate where

import AST.Canonical hiding (_module, _type, _pattern, _expr, _decls, _def, _alias, _binop, _union, _ctor, _docs, _exports, _moduName, _name, _fieldName, _tName, _ctorName)
import East.Multiplate
import Control.Arrow
import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Map (Map)

import qualified AST.Utils.Binop as Binop
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R

-- TODO: this is for an old ast (v0.18?) so needs to be replaced with the 0.19 can ast impl :)

-- NOTE CAVEATS:
-- PBool has a cached bool that isn't updated automatically if union is changed
--

-- Multiplate
data Plate f = Plate
  { _module          :: Module -> f Module
  , _type            :: Type -> f Type
  , _pattern         :: Pattern -> f Pattern
  , _expr            :: Expr -> f Expr
  , _decls           :: Decls -> f Decls
  , _def             :: Def -> f Def
  , _alias           :: Alias -> f Alias
  , _binop           :: Binop -> f Binop
  , _union           :: Union -> f Union
  , _ctor            :: Ctor -> f Ctor
  , _docs            :: Docs -> f Docs
  , _exports         :: Exports -> f Exports
  , _annot           :: Annotation -> f Annotation
  -- not really types, but useful
  , _moduName        :: ModuleName.Canonical -> f ModuleName.Canonical
  , _name            :: N.Name -> f N.Name
  , _pName           :: N.Name -> f N.Name
  , _fieldName       :: N.Name -> f N.Name
  , _tName           :: N.Name -> f N.Name
  , _ctorName        :: N.Name -> f N.Name
  }


type Set a = Map a ()
type ModuName = N.Name
type VarName = N.Name
type FieldName = N.Name
type TName = N.Name
type CtorName = N.Name

type RecordName = N.Name

instance Multiplate Plate where
  multiplate f =
    Plate b_module b_type b_pattern b_expr b_decls b_def b_alias b_binop b_union b_ctor b_docs b_exports b_annot b_moduName b_name b_pName b_fieldName b_tName b_ctorName
    where
      b_module m =
        case m of
          Module modu docs exports decls _unions _aliases _binops effects -> Module <$> _moduName f modu <*> _docs f docs <*> _exports f exports
            <*> _decls f decls
            <*> traverseMap (_name f, _union f) _unions
            <*> traverseMap (_name f, _alias f) _aliases
            <*> traverseMap (_name f, _binop f) _binops
            <*> pure effects

      b_type t =
        case t of
          (TLambda t1 t2) -> TLambda <$> _type f t1 <*> _type f t2
          (TVar n) -> TVar <$> pure n
          (TType modu n tipes) -> TType <$> _moduName f modu <*> _tName f n <*> traverse (_type f) tipes
          (TRecord fields mName) -> TRecord <$> traverseMap (_fieldName f, (\(FieldType w t) -> FieldType <$> pure w <*> _type f t)) fields <*> traverse (_name f) mName
          (TUnit) -> pure TUnit
          (TTuple t1 t2 mt3) -> TTuple <$> _type f t1 <*> _type f t2 <*> traverse (_type f) mt3
          (TAlias modu n tvarTypes aliasType) ->
            case aliasType of
              Holey at -> TAlias <$> _moduName f modu <*> pure n <*> traversePairs (pure, _type f) tvarTypes <*> (Holey <$> _type f at)
              Filled at -> TAlias <$> _moduName f modu <*> pure n <*> traversePairs (pure, _type f) tvarTypes <*> (Filled <$> _type f at)

      b_pattern p = traverseLocated b_pattern' p

      b_pattern' p =
        case p of
          (PAnything) -> pure PAnything
          (PVar n) -> PVar <$> pure n
          (PRecord fields) -> PRecord <$> traverse (_fieldName f) fields
          (PAlias p n) -> PAlias <$> _pattern f p <*> _pName f n
          (PUnit) -> pure PUnit
          (PTuple p1 p2 mp3) -> PTuple <$> _pattern f p1 <*> _pattern f p2 <*> traverse (_pattern f) mp3
          (PList pats) -> PList <$> traverse (_pattern f) pats
          (PCons p1 p2) -> PCons <$> _pattern f p1 <*> _pattern f p2
          (PBool union bool) -> PBool <$> _union f union <*> pure bool
          (PChr t) -> PChr <$> pure t
          (PStr t) -> PStr <$> pure t
          (PInt i) -> PInt <$> pure i
          (PCtor _p_home _p_type _p_union _p_name _p_index _p_args) ->
            PCtor <$> _moduName f _p_home <*> _tName f _p_type <*> _union f _p_union <*> _pName f _p_name <*> pure _p_index <*> traverse parg _p_args
            where
              parg (PatternCtorArg _index t _arg) =
                PatternCtorArg <$> pure _index <*> _type f t <*> _pattern f _arg

      b_expr e = traverseLocated b_expr' e

      b_expr' e =
        case e of
          (VarLocal n) -> VarLocal <$> _name f n
          (VarTopLevel modu n) -> VarTopLevel <$> _moduName f modu <*> _name f n
          (VarKernel kName n) -> VarKernel <$> pure kName <*> _name f n
          (VarForeign modu n annot) -> VarForeign <$> _moduName f modu <*> _name f n <*> _annot f annot
          (VarCtor ctorOpts modu n idx annot) -> VarCtor <$> pure ctorOpts <*> _moduName f modu <*> _name f n <*> pure idx <*> _annot f annot
          (VarDebug modu n annot) -> VarDebug <$> _moduName f modu <*> _name f n <*> _annot f annot
          (VarOperator sym modu n annot) -> VarOperator <$> _name f sym <*> _moduName f modu <*> _name f n <*> _annot f annot
          (Chr t) -> Chr <$> pure t
          (Str t) -> Str <$> pure t
          (Int i) -> Int <$> pure i
          (Float d) -> Float <$> pure d
          (List exprs) -> List <$> traverse (_expr f) exprs
          (Negate e) -> Negate <$> _expr f e
          (Binop sym modu n annot e1 e2) -> Binop <$> _name f sym <*> _moduName f modu <*> _name f n <*> _annot f annot <*> _expr f e1 <*> _expr f e2
          (Lambda pats e) -> Lambda <$> traverse (_pattern f) pats <*> _expr f e
          (Call e args) -> Call <$> _expr f e <*> traverse (_expr f) args
          (If cases elseExpr) -> If <$> (traversePairs (_expr f, _expr f) cases) <*> _expr f elseExpr
          (Let def e) -> Let <$> _def f def <*> _expr f e
          (LetRec defs e) -> LetRec <$> traverse (_def f) defs <*> _expr f e
          (LetDestruct p e1 e2) -> LetDestruct <$> _pattern f p <*> _expr f e1 <*> _expr f e2
          (Case e branches) -> Case <$> _expr f e <*> traverse (\(CaseBranch p e) -> CaseBranch <$> _pattern f p <*> _expr f e) branches
          (Accessor n) -> Accessor <$> _fieldName f n
          (Access e atN) -> Access <$> _expr f e <*> traverseLocated (_fieldName f) atN
          (Update n e updatedFields) -> Update <$> pure n <*> _expr f e <*> (traverseMap (_fieldName f, (\(FieldUpdate r e) -> FieldUpdate <$> pure r <*> _expr f e)) updatedFields)
          (Record fields) -> Record <$> traverseMap (_fieldName f, _expr f) fields
          (Unit) -> pure Unit
          (Tuple e1 e2 me3) -> Tuple <$> _expr f e1 <*> _expr f e2 <*> traverse (_expr f) me3
          (Shader t1 t2 shader) -> pure (Shader t1 t2 shader)

      b_decls d =
        case d of
          Declare def decls -> Declare <$> _def f def <*> _decls f decls
          DeclareRec defs decls -> DeclareRec <$> traverse (_def f) defs <*> _decls f decls
          SaveTheEnvironment -> pure SaveTheEnvironment

      b_def d =
        case d of
          Def atN pats e -> Def <$> traverseLocated (_name f) atN <*> traverse (_pattern f) pats <*> _expr f e
          TypedDef atN freeVars patTipes e t -> TypedDef <$> traverseLocated (_name f) atN <*> pure freeVars <*> traversePairs (_pattern f, _type f) patTipes <*> _expr f e <*> _type f t

      b_alias a =
        case a of
          Alias freevars t -> Alias <$> pure freevars <*> _type f t

      b_binop b =
        case b of
          Binop_ assoc prec n -> Binop_ assoc prec <$> _name f n

      b_union u =
        case u of
          Union freevars ctors _u_numAlts _u_opts -> Union <$> pure freevars <*> traverse (_ctor f) ctors <*> pure _u_numAlts <*> pure _u_opts

      b_ctor c =
        case c of
          Ctor n idx argCount types -> Ctor <$> _name f n <*> pure idx <*> pure argCount <*> traverse (_type f) types

      b_docs d =
        case d of
          NoDocs region -> NoDocs <$> pure region
          YesDocs _region _overview _comments -> YesDocs <$> pure _region <*> pure _overview <*> traverseMap (_name f, pure) _comments

      b_exports e =
        pure e

      b_annot a =
        case a of
          Forall freeVars t -> Forall <$> traverseMap (_name f, pure) freeVars <*> _type f t

      b_moduName n = pure n
      b_name n = pure n
      b_pName n = pure n
      b_fieldName n = pure n
      b_tName n = pure n
      b_ctorName n = pure n


  mkPlate f =
    Plate
      (f _module)
      (f _type)
      (f _pattern)
      (f _expr)
      (f _decls)
      (f _def)
      (f _alias)
      (f _binop)
      (f _union)
      (f _ctor)
      (f _docs)
      (f _exports)
      (f _annot)
      (f _moduName)
      (f _name)
      (f _pName)
      (f _fieldName)
      (f _tName)
      (f _ctorName)

sequenceA2 :: Applicative f => (f a, f b) -> f (a, b)
sequenceA2 (a, b) = liftA2 (,) a b

traverseMap :: (Applicative f, Ord a, Ord a') => (a -> f a', b -> f b') -> Map a b -> f (Map a' b')
traverseMap (f, g) m =
  Map.fromList <$>
  traverse (\(a,b) -> liftA2 (,) (f a) (g b))
  (Map.toList m)

traversePairs :: Applicative f => (a -> f a', b -> f b') -> [(a, b)] -> f [(a', b')]
traversePairs (f, g) =
  traverse (\(a,b) -> liftA2 (,) (f a) (g b))

traverseLocated :: Applicative f => (a -> f b) -> (A.Located a) -> f (A.Located b)
traverseLocated f (A.At r n) =
  A.At r <$> f n
