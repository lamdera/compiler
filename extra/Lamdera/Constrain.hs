{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Constrain where

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Name as N
import Type.Type as Type hiding (Descriptor(..))

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath as FP ((</>))

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
import Data.IORef
import System.IO.Unsafe

import Lamdera
import Lamdera.Project (lamderaCore)


constrain pkg modu name freevars t region expected =
  if pkg == lamderaCore && modu == "Lamdera" && name == "sendToBackend" then
    do
      sourcePkg <- determineTypesLocation "ToBackend"
      let
        fn (Can.TVar "toBackend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToBackend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

  else if pkg == lamderaCore && modu == "Lamdera" && name == "sendToFrontend" then
    do
      sourcePkg <- determineTypesLocation "ToFrontend"
      let
        fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToFrontend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

  else if pkg == lamderaCore && modu == "Lamdera" && name == "broadcast" then
    do
      sourcePkg <- determineTypesLocation "ToFrontend"
      let
        fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToFrontend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

  -- Since we don't have type annotations on codecs, we have exposed top-level
  -- things without type annotations, which means that we see bugs in the type
  -- inference engine that shouldn't be there. This is a hotfix for such a bug,
  -- where the forall. part of the type doesn't hold all the tvars used in the
  -- type, so we inject any missning tvars and hope it works out.
  -- @LAMDERA todo legacy from Haskell transpilation, re-evalutate and remove
  else
    pure $ CForeign region name (Can.Forall (freevars <> getFreevars t) t) expected -- NOTE: prefer freevars over getFreevars if there's a conflict


determineTypesLocation :: Text -> IO N.Name
determineTypesLocation tipe = do

  knownSource <- typeLocation tipe

  case knownSource of
    Just source ->
      pure source

    Nothing -> do

      root <- getProjectRoot
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

      debug $ "looked up types location for " <> T.unpack tipe <> ": " <> show res

      rememberTypeLocation tipe res
      pure res


{-# NOINLINE typeLocations #-}
typeLocations :: IORef (Map.Map Text N.Name)
typeLocations = unsafePerformIO $ newIORef Map.empty


rememberTypeLocation :: Text -> N.Name -> IO ()
rememberTypeLocation str d = atomicModifyIORef typeLocations (\m -> (Map.insert str d m, ()))


typeLocation :: Text -> IO (Maybe N.Name)
typeLocation tipe = do
  fs <- readIORef typeLocations
  pure $ Map.lookup tipe fs


getFreevars :: Can.Type -> Map.Map N.Name ()
getFreevars t =
  Map.fromList $ fmap (\k -> (k, ())) (actualFreeVars t)


actualFreeVars :: Can.Type -> [N.Name]
actualFreeVars t =
  case t of
    Can.TLambda t1 t2 ->
      actualFreeVars t1 <> actualFreeVars t2

    Can.TVar name ->
      [name]

    Can.TType _ _ types ->
      types & fmap actualFreeVars & concat

    Can.TRecord keyTypes mName ->
      keyTypes & Map.map (\(Can.FieldType n t_) -> actualFreeVars t_) & Map.elems & concat

    Can.TUnit ->
      []

    Can.TTuple t1 t2 mt3 ->
      case mt3 of
        Just t3 ->
          actualFreeVars t1 <> actualFreeVars t2 <> actualFreeVars t3
        Nothing ->
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

    Can.TTuple t1 t2 mt3 ->
      case mt3 of
        Just t3 ->
          Can.TTuple (mapTvars fn t1) (mapTvars fn t2) (Just $ mapTvars fn t3)
        Nothing ->
          Can.TTuple (mapTvars fn t1) (mapTvars fn t2) mt3

    Can.TAlias moduleName name keyVals alias ->
      case alias of
        Can.Holey t_ ->
          Can.TAlias moduleName name (keyVals & fmap (\(n, tx) -> (n, mapTvars fn tx))) (Can.Holey $ mapTvars fn t_)

        Can.Filled t_ ->
          Can.TAlias moduleName name (keyVals & fmap (\(n, tx) -> (n, mapTvars fn tx))) (Can.Filled $ mapTvars fn t_)
