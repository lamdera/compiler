{-# OPTIONS_GHC -Wall #-}
module Transpile.Deriving where

{-| This module handles the deriving of type class instances. -}

import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Transpile.Imports as Imports
import qualified Reporting.Region as RR
import qualified Data.Set as Set

import Data.Text (pack)
import Data.List (intercalate, isPrefixOf)
import Data.Monoid
import Data.Function ((&))


addDeriving :: [Hs.Decl] -> [Hs.Decl]
addDeriving =
  let
    f (dataDecl@(Hs.DataDecl a b declHead tags derives)) | containsFns dataDecl == True =
      Hs.DataDecl a b declHead tags [derivingStmt ["Haskelm.Core.Show", "Haskelm.Core.Eq", "ElmVal'"]]
    f (dataDecl@(Hs.DataDecl a b declHead tags derives)) | containsFns dataDecl == False =
      Hs.DataDecl a b declHead tags [derivingStmt ["Haskelm.Core.Show", "Haskelm.Core.Eq", "ElmVal'"]]
    f decl = decl

    derivingStmt things = Hs.Deriving Nothing (fmap (\x -> Hs.IRule Nothing Nothing (Hs.IHCon (Hs.UnQual (Hs.Ident x)))) things)
  in
    fmap f

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

-- this part of the code is supposed to add type constraints for ElmVal', Ord and Monoid (Show+Eq, comparable and appendable)
-- addTypeConstraints?
-- 1. get a list of all types
-- 2. figure our which ones start with "comparable" or "appendable"
-- 3. pair them up with the relevant type constraint and return those as Asst (ClassA assertions)
addTypeConstraints :: [Hs.Decl] -> [Hs.Decl]
addTypeConstraints =
  let
    f (Hs.TypeSig name t) =
      Hs.TypeSig name (addTypeConstraintsForType t)
    f decl = decl

    derivingStmt things = Hs.Deriving Nothing (fmap (\x -> Hs.IRule Nothing Nothing (Hs.IHCon (Hs.UnQual (Hs.Ident x)))) things)
  in
    fmap f


addTypeConstraintsForType :: Hs.Type -> Hs.Type
addTypeConstraintsForType t@(Hs.TyForall _ _ _) = t
addTypeConstraintsForType t =
  case getTypeConstraints t of
    [] ->
      t

    constraints ->
      Hs.TyForall Nothing (Just (Hs.CxTuple $ (Set.toList $ Set.fromList constraints))) t


getTypeConstraints :: Hs.Type -> [Hs.Asst]
getTypeConstraints t =
  tyFoldMap f [t] & concatMap tConstraint
  where
    f (Hs.TyVar (Hs.Ident x)) = [x]
    f _ = []

tConstraint :: String -> [Hs.Asst]
tConstraint n =
  [(Hs.ClassA
      (Hs.UnQual
        (Hs.Ident "ElmVal'")
      )
      [ Hs.TyVar
          (Hs.Ident n)
      ]
  )]
  <>
    if isPrefixOf "comparable" n then
      [(Hs.ClassA
        (Hs.Qual
          (Hs.ModuleName "Haskelm.Core")
          (Hs.Ident "Ord")
        )
        [ Hs.TyVar
            (Hs.Ident n)
        ]
      )]
    else if isPrefixOf "appendable" n then
      [(Hs.ClassA
        (Hs.Qual
          (Hs.ModuleName "Haskelm.Core")
          (Hs.Ident "Monoid")
        )
        [ Hs.TyVar
            (Hs.Ident n)
        ]
      )]
    else
      []

