{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Transpile.Deriving where

{-| This module handles the deriving of type class instances. -}

import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified AST.Canonical as C
import qualified Elm.Name as N

import Data.Monoid
import Data.Function ((&))

type List a = [a]

-- if the adt contains a single function, we cannot derive Eq, so let's look for functions:
containsFns :: Hs.Decl -> Bool
containsFns (Hs.DataDecl _ _ dataType cs _) =
  any cContainsFns cs
containsFns a = False

cContainsFns :: Hs.QualConDecl -> Bool
cContainsFns (Hs.QualConDecl _ _ (Hs.ConDecl _ t)) =
  let
    containsFn (Hs.TyFun _ _) = True
    containsFn _ = False
  in
    tyContains containsFn t
cContainsFns _ = False

tyContains :: (Hs.Type -> Bool) -> [Hs.Type] -> Bool -- traversable foldMap?
tyContains f ts = any (tyContains' f) ts

tyContains' :: (Hs.Type -> Bool) -> Hs.Type -> Bool
tyContains' f t | f t == True = True
tyContains' f (Hs.TyForall _ _ t) = tyContains' f t
tyContains' f (Hs.TyFun t1 t2) = tyContains' f t1 || tyContains' f t2
tyContains' f (Hs.TyTuple _ ts) = tyContains f ts
tyContains' f (Hs.TyList t) =  tyContains' f t
tyContains' f (Hs.TyParArray t) = tyContains' f t
tyContains' f (Hs.TyApp t1 t2) = tyContains' f t1 || tyContains' f t2
tyContains' f (Hs.TyParen t) = tyContains' f t
tyContains' f (Hs.TyInfix t1 _ t2) = tyContains' f t1 || tyContains' f t2
tyContains' f (Hs.TyKind t _) = tyContains' f t
tyContains' f (Hs.TyEquals t1 t2) = tyContains' f t1 || tyContains' f t2
tyContains' f (Hs.TyBang _ _ t) = tyContains' f t
tyContains' _ (Hs.TyVar _) = False
tyContains' _ (Hs.TyCon _) = False
tyContains' _ (Hs.TySplice _) = False
tyContains' _ (Hs.TyPromoted _) = False
tyContains' _ (Hs.TyWildCard _) = False
tyContains' _ (Hs.TyQuasiQuote _ _) = False


tyFoldMap :: Monoid m => (Hs.Type -> m) -> [Hs.Type] -> m -- traversable foldMap?
tyFoldMap f ts = fmap (tyFoldMapHelper' f) ts & mconcat

tyFoldMap' :: Monoid m => (Hs.Type -> m) -> Hs.Type -> m
-- tyFoldMap' f t | f t == True = True
tyFoldMap' f (Hs.TyForall _ _ t) = tyFoldMapHelper' f t
tyFoldMap' f (Hs.TyFun t1 t2) = tyFoldMapHelper' f t1 <> tyFoldMapHelper' f t2
tyFoldMap' f (Hs.TyTuple _ ts) = tyFoldMap f ts
tyFoldMap' f (Hs.TyList t) =  tyFoldMapHelper' f t
tyFoldMap' f (Hs.TyParArray t) = tyFoldMapHelper' f t
tyFoldMap' f (Hs.TyApp t1 t2) = tyFoldMapHelper' f t1 <> tyFoldMapHelper' f t2
tyFoldMap' f (Hs.TyParen t) = tyFoldMapHelper' f t
tyFoldMap' f (Hs.TyInfix t1 _ t2) = tyFoldMapHelper' f t1 <> tyFoldMapHelper' f t2
tyFoldMap' f (Hs.TyKind t _) = tyFoldMapHelper' f t
tyFoldMap' f (Hs.TyEquals t1 t2) = tyFoldMapHelper' f t1 <> tyFoldMapHelper' f t2
tyFoldMap' f (Hs.TyBang _ _ t) = tyFoldMapHelper' f t
tyFoldMap' f a = f a

tyFoldMapHelper' :: Monoid m => (Hs.Type -> m) -> Hs.Type -> m
tyFoldMapHelper' f t = f t <> tyFoldMap' f t



----- Figure out type constraints


addTypeConstraintsForType :: C.Type -> Hs.Type -> Hs.Type
addTypeConstraintsForType ct t@(Hs.TyForall _ _ _) = t
addTypeConstraintsForType ct t =
  case typeVariables ct & fmap N.toText & concatMap tConstraint of
    [] ->
      t

    constraints ->
      Hs.TyForall Nothing (Just (Hs.CxTuple $ (Set.toList $ Set.fromList constraints))) t


typeVariables :: C.Type -> List N.Name
typeVariables t =
  case t of
    (C.TUnit) -> []
    (C.TVar name) | "number" `Text.isPrefixOf` N.toText name -> []
    (C.TVar name) -> [name]
    (C.TType moduleName name types) -> []
    (C.TRecord nameFieldTypeMap mName) ->
      nameFieldTypeMap
      & Map.elems
      & fmap (\(C.FieldType _ t) -> t)
      & concatMap typeVariables
    (C.TLambda t1 t2) -> concatMap typeVariables [t1, t2]
    (C.TTuple t1 t2 Nothing) -> concatMap typeVariables [t1, t2]
    (C.TTuple t1 t2 (Just t3)) -> concatMap typeVariables [t1, t2, t3]
    (C.TAlias moduleName name nameTypeList aliasType) ->
      case aliasType of
        C.Holey t1 -> typeVariables t1
        C.Filled t1 -> typeVariables t1


tConstraint :: Text.Text -> [Hs.Asst]
tConstraint n =
  let
    classA n tc =
      (Hs.ClassA
        (Hs.Qual
          (Hs.ModuleName "Lamdera.Haskelm.Core")
          (Hs.Ident tc)
        )
        [ Hs.TyVar
          (Hs.Ident (Text.unpack n))
        ]
      )
    tcs n =
      (if Text.isPrefixOf "compappend" n ||
         Text.isPrefixOf "comparable" n then ["Ord"] else [])
      ++
      (if Text.isPrefixOf "compappend" n ||
         Text.isPrefixOf "appendable" n then ["Monoid"] else [])
  in
    classA n <$> ("ElmVal'" : tcs n)
