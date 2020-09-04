{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Constrain where

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Name as N
import Type.Type as Type hiding (Descriptor(..))

import qualified Data.Text as T
import Control.Lens.Plated (transform)
import Control.Lens ((^..), to)
import Control.Lens.Plated
import Control.Lens.Prism
import qualified Data.Map.Strict as Map
import System.FilePath as FP ((</>))

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
import Data.IORef
import System.IO.Unsafe

import Lamdera


constrain pkg modu name freevars t region expected =
  if pkg == Pkg.lamderaCore && modu == "Lamdera" && name == "sendToBackend" then
    do
      sourcePkg <- determineTypesLocation "ToBackend"
      let
        fn (Can.TVar "toBackend") = Can.TType (ModuleName.Canonical Pkg.dummyName (N.Name sourcePkg)) "ToBackend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (transform fn t)) expected

  else if pkg == Pkg.lamderaCore && modu == "Lamdera" && name == "sendToFrontend" then
    do
      sourcePkg <- determineTypesLocation "ToFrontend"
      let
        fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName (N.Name sourcePkg)) "ToFrontend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (transform fn t)) expected

  else if pkg == Pkg.lamderaCore && modu == "Lamdera" && name == "broadcast" then
    do
      sourcePkg <- determineTypesLocation "ToFrontend"
      let
        fn (Can.TVar "toFrontend") = Can.TType (ModuleName.Canonical Pkg.dummyName (N.Name sourcePkg)) "ToFrontend" []
        fn x = x
      pure $ CForeign region name (Can.Forall freevars (transform fn t)) expected

  -- Since we don't have type annotations on codecs, we have exposed top-level
  -- things without type annotations, which means that we see bugs in the type
  -- inference engine that shouldn't be there. This is a hotfix for such a bug,
  -- where the forall. part of the type doesn't hold all the tvars used in the
  -- type, so we inject any missning tvars and hope it works out.
  -- @LAMDERA todo legacy from Haskell transpilation, re-evalutate and remove
  else
    pure $ CForeign region name (Can.Forall (freevars <> getFreevars t) t) expected -- NOTE: prefer freevars over getFreevars if there's a conflict


determineTypesLocation :: Text -> IO Text
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

      debug $ "looked up types location for " <> T.unpack tipe <> ": " <> T.unpack res

      rememberTypeLocation tipe res
      pure res


{-# NOINLINE typeLocations #-}
typeLocations :: IORef (Map.Map Text Text)
typeLocations = unsafePerformIO $ newIORef Map.empty


rememberTypeLocation :: Text -> Text -> IO ()
rememberTypeLocation str d = atomicModifyIORef typeLocations (\m -> (Map.insert str d m, ()))


typeLocation :: Text -> IO (Maybe Text)
typeLocation tipe = do
  fs <- readIORef typeLocations
  pure $ Map.lookup tipe fs


getFreevars t =
  let
    actualFreeVars :: Can.Type -> [N.Name]
    actualFreeVars e =
      let
        fn (Can.TVar n) = Just n
        fn _ = Nothing
      in
        e ^.. cosmos . to fn . _Just
  in
  Map.fromList $ fmap (\k -> (k, ())) (actualFreeVars t)
