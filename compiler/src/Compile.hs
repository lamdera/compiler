{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Reporting.Error as E
import qualified Reporting.Result as R
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type

-- import System.IO.Unsafe (unsafePerformIO)


-- @DEPRECATED for alpha12 release migration only
import qualified Lamdera.Wire2.Core
import qualified Lamdera.Wire2.Interfaces

import qualified Lamdera.Wire3.Core
import qualified Lamdera.Wire3.Interfaces
import qualified Lamdera.Wire3.Helpers as Lamdera.Wire
import Lamdera
import qualified CanSer.CanSer as ToSource
import qualified Data.Text as T
import qualified Data.Utf8

-- import StandaloneInstances

-- COMPILE


data Artifacts =
  Artifacts
    { _modul :: Can.Module
    , _types :: Map.Map Name.Name Can.Annotation
    , _graph :: Opt.LocalGraph
    }


compile :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile pkg ifaces modul = do
  -- @TEMPORARY debugging
  -- Inject stub definitions for wire functions, so the canonicalize phase can run
  -- Necessary for user-code which references yet-to-be generated functions
  let modul_ =
        modul
          & Lamdera.Wire3.Interfaces.modifyModul pkg ifaces
          & Lamdera.Wire2.Interfaces.modifyModul pkg ifaces
      moduleName = T.pack $ Data.Utf8.toChars $ Src.getName modul

  -- ()          <- debugPassText "starting canonical" "" (pure ())
  canonical0  <- canonicalize pkg ifaces modul_
  -- ()          <- debugPassText "starting canonical2" moduleName (pure ())

  -- Add Canonical Wire gens, i.e. the `w2_[en|de]code_TYPENAME` functions
  canonical1 <- Lamdera.Wire3.Core.addWireGenerations canonical0 pkg ifaces modul_
  canonical2 <- Lamdera.Wire2.Core.addWireGenerations canonical1 pkg ifaces modul_

  -- () <- unsafePerformIO $ do
  --   case (pkg, Src.getName modul) of
  --     ((Pkg.Name "author" "project"), "Page") -> do
  --       -- canonical_
  --       --   & Can._decls
  --       --   & Lamdera.Wire.Helpers.findDef "testing"
  --       --   & formatHaskellValue "Compile.findDef"
  --       atomicPutStrLn $ T.unpack $ T.take 5000 $ ToSource.convert canonical2
  --       pure ()
  --     _ ->
  --       pure ()
  --   -- writeUtf8 "canprinted_without.txt" (hindentFormatValue canonical_)
  --   pure (pure ())

  -- ()          <- debugPassText "starting typecheck" moduleName (pure ())
  annotations <- typeCheck modul_ canonical2
  -- ()          <- debugPassText "starting nitpick" moduleName (pure ())
  ()          <- nitpick canonical2
  -- ()          <- debugPassText "starting optimize" moduleName (pure ())
  objects     <- optimize modul_ annotations canonical2
  return (Artifacts canonical2 annotations objects)


{- The original compile function for reference -}
compile_ :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile_ pkg ifaces modul =
  do  canonical   <- canonicalize pkg ifaces modul
      annotations <- typeCheck modul canonical
      ()          <- nitpick canonical
      objects     <- optimize modul annotations canonical
      return (Artifacts canonical annotations objects)


-- PHASES


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
canonicalize pkg ifaces modul =
  case snd $ R.run $ Canonicalize.canonicalize pkg ifaces modul of
    Right canonical ->
      Right canonical

    Left errors ->
      Left $ E.BadNames errors


typeCheck :: Src.Module -> Can.Module -> Either E.Error (Map.Map Name.Name Can.Annotation)
typeCheck modul canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      Right annotations

    Left errors ->
      Left (E.BadTypes (Localizer.fromModule modul) errors)


nitpick :: Can.Module -> Either E.Error ()
nitpick canonical =
  case PatternMatches.check canonical of
    Right () ->
      Right ()

    Left errors ->
      Left (E.BadPatterns errors)


optimize :: Src.Module -> Map.Map Name.Name Can.Annotation -> Can.Module -> Either E.Error Opt.LocalGraph
optimize modul annotations canonical =
  case snd $ R.run $ Optimize.optimize annotations canonical of
    Right localGraph ->
      Right localGraph

    Left errors ->
      Left (E.BadMains (Localizer.fromModule modul) errors)
