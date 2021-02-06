{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Canonical where

{- Helpers for loading cached interfaces after/outside compilation process -}

import Control.Monad (liftM2)
import qualified Data.Map as Map
import qualified Data.OneOrMore as OneOrMore
import System.FilePath ((</>))
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Name

import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Compile
import qualified Parse.Module as Parse

import qualified Lamdera.Interfaces
import Lamdera

import Lamdera.Wire3.Helpers (findDef)

-- @TODO
-- Lamdera.Canonical.loadDef "src/Test/Basic.elm" "exampleFunction"


loadSingleCanonical :: FilePath -> IO Can.Module
loadSingleCanonical path = do
  (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts path
  pure canonical


loadSingleObjects :: FilePath -> IO Opt.LocalGraph
loadSingleObjects path = do
  (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts path
  pure objects


loadSingleArtifacts :: FilePath -> IO Compile.Artifacts
loadSingleArtifacts path = do
  ifaces <- Lamdera.Interfaces.all [path]

  source <- File.readUtf8 path
  case Parse.fromByteString Parse.Application source of
    Right modul ->
      case Compile.compile Pkg.dummyName ifaces modul of
        Right artifacts ->
          pure artifacts

        Left err -> error $ "error!" ++ show err

    Left err ->
      error "bad syntax"



showDefOptimized :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefOptimized project file name = do
  withCurrentDirectory project $ do

    -- canonical <- Lamdera.Canonical.loadSingleCanonical file

    objects <- Lamdera.Canonical.loadSingleObjects file

        -- objects
        --   & _l_nodes
        --   &

    objects
      & Opt._l_nodes
      -- & Map.lookup name
      & Map.filterWithKey (\k _ ->
          case k of
            Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
              name_ == name
        )
      & formatHaskellValue ("found")


showDefCanonical :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefCanonical project file name = do
  withCurrentDirectory project $ do
    canonical <- Lamdera.Canonical.loadSingleCanonical file
    -- objects <- Lamdera.Canonical.loadSingleObjects file

    -- objects
    --   & Opt._l_nodes
    --   -- & Map.lookup name
    --   -- & Map.filterWithKey (\k _ ->
    --   --     case k of
    --   --       Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
    --   --         name_ == name
    --   --   )
    --   & formatHaskellValue (T.pack $ file <> Data.Name.toChars name)


    -- formatHaskellValue "canonical Test.Basic" canonical
    --
    case canonical & Can._decls & findDef name of
      Just def -> do
        formatHaskellValue "suite def" def
        -- formatHaskellValue "suite:" $ run def Map.empty

      Nothing ->
        putStrLn "no suite found"

    pure ()


showDefAnnotation :: FilePath -> FilePath -> Data.Name.Name -> IO ()
showDefAnnotation project file name = do
  withCurrentDirectory project $ do
    (Compile.Artifacts canonical annotations objects) <- loadSingleArtifacts file

    -- , _types :: Map.Map Name.Name Can.Annotation

    annotations
      & Map.lookup name
      & formatHaskellValue ("found")

    pure ()
