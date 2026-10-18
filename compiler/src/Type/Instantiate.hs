{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Type.Instantiate
  ( FreeVars
  , fromSrcType
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map

import qualified AST.Canonical as Can
import qualified AST.Prim.TypeVar as T
import Type.Type



-- FREE VARS


type FreeVars =
  Map.Map T.Var Type



-- FROM SOURCE TYPE


fromSrcType :: Map.Map T.Var Type -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
  -- @LAMDERA built lazily: the solver defers alias bodies, and the parts of closed
  -- types (Type.Type.Deferred), so most of a big annotation is never looked at.
  -- Building it has no effects; the IO is only there for the shape of this function.
  return (fromSrcTypeLazy freeVars sourceType)


fromSrcTypeLazy :: Map.Map T.Var Type -> Can.Type -> Type
fromSrcTypeLazy freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      FunN
        (fromSrcTypeLazy freeVars arg)
        (fromSrcTypeLazy freeVars result)

    Can.TVar name ->
      $(Map.require 'fromSrcType) name freeVars T.varToChars

    Can.TType home name args ->
      AppN home name (map (fromSrcTypeLazy freeVars) args)

    Can.TAlias home name args aliasedType ->
      let
        targs = map (fmap (fromSrcTypeLazy freeVars)) args

        body =
          case aliasedType of
            Can.Filled realType ->
              fromSrcTypeLazy freeVars realType

            Can.Holey realType ->
              fromSrcTypeLazy (Map.fromList targs) realType

        -- @LAMDERA where it came from, see Type.Type.DeferredKey. Only when the
        -- arguments as written have no type variables: inside an alias body they are
        -- the body's parameters (`Html msg` in `type alias Config msg = { view : Html
        -- msg }`), and the same written arguments then stand for different types in
        -- different uses, so they would not decide the body.
        source =
          if all (isClosedCanType . snd) args then
            case aliasedType of
              Can.Filled realType -> Just (realType, False, map snd args)
              Can.Holey realType -> Just (realType, True, map snd args)
          else
            Nothing
      in
      AliasN home name targs body source

    Can.TPair a b ->
      PairN
        (fromSrcTypeLazy freeVars a)
        (fromSrcTypeLazy freeVars b)

    Can.TTriple a b c ->
      TripleN
        (fromSrcTypeLazy freeVars a)
        (fromSrcTypeLazy freeVars b)
        (fromSrcTypeLazy freeVars c)

    Can.TUnit ->
      UnitN

    Can.TRecord fields maybeExt ->
      RecordN
        (Map.map (fromSrcFieldType freeVars) fields)
        (case maybeExt of
          Nothing  -> EmptyRecordN
          Just ext -> $(Map.require 'fromSrcType) ext freeVars T.varToChars
        )


fromSrcFieldType :: Map.Map T.Var Type -> Can.FieldType -> Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
  fromSrcTypeLazy freeVars tipe


-- @LAMDERA Whether a type as written has no type variables. An alias's real type only
-- has the type variables of its arguments, so it is not looked at.
isClosedCanType :: Can.Type -> Bool
isClosedCanType tipe =
  case tipe of
    Can.TLambda a b -> isClosedCanType a && isClosedCanType b
    Can.TVar _ -> False
    Can.TType _ _ args -> all isClosedCanType args
    Can.TRecord fields maybeExt ->
      maybeExt == Nothing && all (\(Can.FieldType _ t) -> isClosedCanType t) fields
    Can.TUnit -> True
    Can.TPair a b -> isClosedCanType a && isClosedCanType b
    Can.TTriple a b c -> isClosedCanType a && isClosedCanType b && isClosedCanType c
    Can.TAlias _ _ args _ -> all (isClosedCanType . snd) args
