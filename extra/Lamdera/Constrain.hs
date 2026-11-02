{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Constrain where

import qualified Lamdera.Compat.Canonical as Can
import qualified Elm.Interface as I
import qualified Array
import qualified AST.Prim.Fields as F
import qualified AST.Prim.TypeIndex as TI
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as TN
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeVar as TV
import Type.Type as Type hiding (Descriptor(..))

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath as FP ((</>))

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
import Data.IORef
import System.IO.Unsafe

import Lamdera
import Lamdera.Project (lamderaCore)


-- @LAMDERA Elm 0.19.4: called for every VarForeign with its home and its annotation, which
-- is an I.Annotation (type variables are indexes into its array of names)
constrain :: ModuleName.Canonical -> N.Name -> I.Annotation -> A.Region -> E.Expected Type -> IO Constraint
constrain home@(ModuleName.Canonical pkg modu) name annotation@(I.Forall freevars t) region expected =
  if pkg == lamderaCore && modu == "Lamdera" then

    case name of
      "sendToBackend" -> do
        sourcePkg <- determineTypesLocation "ToBackend"
        pure $ CForeign region name (I.Forall freevars (replaceVar freevars "toBackend" (userType sourcePkg "ToBackend") t)) expected

      "sendToFrontend" -> do
        sourcePkg <- determineTypesLocation "ToFrontend"
        pure $ CForeign region name (I.Forall freevars (replaceVar freevars "toFrontend" (userType sourcePkg "ToFrontend") t)) expected

      "broadcast" -> do
        sourcePkg <- determineTypesLocation "ToFrontend"
        pure $ CForeign region name (I.Forall freevars (replaceVar freevars "toFrontend" (userType sourcePkg "ToFrontend") t)) expected

      _ ->
        pure $ CForeign region name annotation expected

  else
    pure $ CForeign region name annotation expected


-- the user's own ToBackend / ToFrontend. A custom type's mask is 0b00 (see M.variants),
-- and an alias for one is not comparable either, as before Elm 0.19.4 when this was a
-- TType too.
userType :: Module.Name -> TN.Name -> I.Type
userType sourcePkg typeName =
  I.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) typeName M.enum Array.empty


-- @LAMDERA was mapTvars with a function matching `Can.TVar "toBackend"`
replaceVar :: Array.Array TV.Var -> TV.Var -> I.Type -> I.Type -> I.Type
replaceVar freevars var replacement tipe =
  let
    go = replaceVar freevars var replacement
  in
  case tipe of
    I.TLambda a b cs -> I.TLambda (go a) (go b) (map go cs)
    I.TVar i -> if TI.get i freevars == var then replacement else tipe
    I.TType h n m xs -> I.TType h n m (Array.map go xs)
    I.TAlias h n m xs aliased -> I.TAlias h n m (Array.map go xs) aliased
    I.TUnit -> tipe
    I.TPair a b -> I.TPair (go a) (go b)
    I.TTriple a b c -> I.TTriple (go a) (go b) (go c)
    I.TEmpty -> tipe
    I.TRecord fs -> I.TRecord (F.map go fs)
    I.TExtRec fs ext -> I.TExtRec (F.map go fs) ext


determineTypesLocation :: Text -> IO Module.Name
determineTypesLocation tipe = do

  knownSource <- typeLocation tipe

  case knownSource of
    Just source ->
      pure source

    Nothing -> do

      root <- getProjectRoot "determineTypesLocation"
      bridgeM <- readUtf8Text $ root </> "src" </> "Bridge.elm"
      let
        res =
          case bridgeM of
            Just bridge ->
              if textContains ("type " <> tipe) bridge then
                "Bridge"
              else
                "Types"

            Nothing ->
              "Types"

      -- debug $ "looked up types location for " <> T.unpack tipe <> ": " <> show res

      rememberTypeLocation tipe res
      pure res


{-# NOINLINE typeLocations #-}
typeLocations :: IORef (Map.Map Text Module.Name)
typeLocations = unsafePerformIO $ newIORef Map.empty

resetTypeLocations :: IO ()
resetTypeLocations = atomicModifyIORef typeLocations (\m -> (Map.empty, ()))

rememberTypeLocation :: Text -> Module.Name -> IO ()
rememberTypeLocation str d = atomicModifyIORef typeLocations (\m -> (Map.insert str d m, ()))


typeLocation :: Text -> IO (Maybe Module.Name)
typeLocation tipe = do
  fs <- readIORef typeLocations
  pure $ Map.lookup tipe fs


getFreevars :: Can.Type -> Map.Map TV.Var ()
getFreevars t =
  Map.fromList $ fmap (\k -> (k, ())) (actualFreeVars t)


actualFreeVars :: Can.Type -> [TV.Var]
actualFreeVars t =
  case t of
    Can.TLambda t1 t2 ->
      actualFreeVars t1 <> actualFreeVars t2

    Can.TVar name ->
      [name]

    Can.TType _ _ types ->
      types & concatMap actualFreeVars

    Can.TRecord keyTypes mName ->
      keyTypes & Map.map (\(Can.FieldType n t_) -> actualFreeVars t_) & Map.elems & concat

    Can.TUnit ->
      []

    Can.TTriple t1 t2 t3 ->
      actualFreeVars t1 <> actualFreeVars t2 <> actualFreeVars t3

    Can.TPair t1 t2 ->
      actualFreeVars t1 <> actualFreeVars t2

    Can.TAlias moduleName name keyVals alias ->
      case alias of
        Can.Holey t_ ->
          (keyVals & fmap (actualFreeVars . snd) & concat) <> (actualFreeVars t_)

        Can.Filled t_ ->
          (keyVals & fmap (actualFreeVars . snd) & concat) <> (actualFreeVars t_)


mapTvars :: (Can.Type -> Can.Type) -> Can.Type -> Can.Type
mapTvars fn t =
  case t of
    Can.TLambda t1 t2 ->
      Can.TLambda (mapTvars fn t1) (mapTvars fn t2)

    Can.TVar name ->
      fn t

    Can.TType moduleName name types ->
      Can.TType moduleName name $ types & fmap (mapTvars fn)

    Can.TRecord keyTypes mName ->
      Can.TRecord (keyTypes & Map.map (\(Can.FieldType n t_) -> Can.FieldType n $ mapTvars fn t_)) mName

    Can.TUnit ->
      t

    Can.TTriple t1 t2 t3 ->
      Can.TTriple (mapTvars fn t1) (mapTvars fn t2) (mapTvars fn t3)

    Can.TPair t1 t2 ->
      Can.TPair (mapTvars fn t1) (mapTvars fn t2)

    Can.TAlias moduleName name keyVals alias ->
      case alias of
        Can.Holey t_ ->
          Can.TAlias moduleName name (keyVals & fmap (\(n, tx) -> (n, mapTvars fn tx))) (Can.Holey $ mapTvars fn t_)

        Can.Filled t_ ->
          Can.TAlias moduleName name (keyVals & fmap (\(n, tx) -> (n, mapTvars fn tx))) (Can.Filled $ mapTvars fn t_)
