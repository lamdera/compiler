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


constrain pkg modu name annotation@(Can.Forall freevars t) region expected =
  if pkg == lamderaCore && modu == "Lamdera" then

    case name of
      "sendToBackend" -> do
        sourcePkg <- determineTypesLocation "ToBackend"
        let
          fn (Can.TVar "toBackend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToBackend" []
          fn x = x
        pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

      "sendToFrontend" -> do
        sourcePkg <- determineTypesLocation "ToFrontend"
        let
          fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToFrontend" []
          fn x = x
        pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

      "broadcast" -> do
        sourcePkg <- determineTypesLocation "ToFrontend"
        let
          fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName sourcePkg) "ToFrontend" []
          fn x = x
        pure $ CForeign region name (Can.Forall freevars (mapTvars fn t)) expected

      _ ->
        pure $ CForeign region name annotation expected

  else
    pure $ CForeign region name annotation expected


determineTypesLocation :: Text -> IO N.Name
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
typeLocations :: IORef (Map.Map Text N.Name)
typeLocations = unsafePerformIO $ newIORef Map.empty

resetTypeLocations :: IO ()
resetTypeLocations = atomicModifyIORef typeLocations (\m -> (Map.empty, ()))

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
      types & concatMap actualFreeVars

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
