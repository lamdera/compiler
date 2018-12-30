{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Transpile.Deriving (addTypeConstraintsForType) where

{-| This module handles the deriving of type class instances. -}

import qualified Data.Text as Text
import qualified Language.Haskell.Exts.Simple.Syntax as Hs
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified AST.Canonical as C
import qualified Elm.Name as N

import Transpile.Reserved (ident)

import Data.Function ((&))

type List a = [a]

----- Figure out type constraints


addTypeConstraintsForType :: C.Type -> Hs.Type -> Hs.Type
addTypeConstraintsForType _ t@(Hs.TyForall _ _ _) = t
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
    (C.TType _ _ types) -> concatMap typeVariables types
    (C.TRecord nameFieldTypeMap _) ->
      nameFieldTypeMap
      & Map.elems
      & fmap (\(C.FieldType _ t) -> t)
      & concatMap typeVariables
    (C.TLambda t1 t2) -> concatMap typeVariables [t1, t2]
    (C.TTuple t1 t2 Nothing) -> concatMap typeVariables [t1, t2]
    (C.TTuple t1 t2 (Just t3)) -> concatMap typeVariables [t1, t2, t3]
    (C.TAlias _ _ nameTypeList aliasType) ->
      case aliasType of
        C.Holey _ -> nameTypeList & fmap snd & concatMap typeVariables
        C.Filled _ -> nameTypeList & fmap snd & concatMap typeVariables


tConstraint :: Text.Text -> [Hs.Asst]
tConstraint n =
  let
    classA n tc =
      (Hs.ClassA
        (Hs.Qual
          (Hs.ModuleName "Lamdera.Haskelm.Core")
          (ident (N.fromText tc))
        )
        [ Hs.TyVar
          (ident (N.fromText n))
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
