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


import qualified Lamdera.Wire
import qualified Lamdera.Wire.Interfaces
import Lamdera
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
  -- Inject stub definitions for wire functions, so the canonicalize phase can run
  -- Necessary for user-code which references yet-to-be generated functions
  let modul_ = Lamdera.Wire.Interfaces.modifyModul pkg ifaces modul
  -- ()          <- debugPassText "starting canonical" "" (pure ())
  canonical  <- canonicalize pkg ifaces modul_
  -- ()          <- debugPassText "starting canonical2" "" (pure ())

  -- Add Canonical Wire gens, i.e. the `w2_[en|de]code_TYPENAME` functions
  canonical_   <- Lamdera.Wire.addWireGenerations canonical pkg ifaces modul_

  -- () <- unsafePerformIO $ do
  --   writeUtf8 "canprinted_without.txt" (hindentFormatValue canonical_)
  --   pure (pure ())

  -- ()          <- debugPassText "starting typecheck" "" (pure ())
  annotations <- typeCheck modul_ canonical_
  -- ()          <- debugPassText "starting nitpick" "" (pure ())
  ()          <- nitpick canonical_
  -- ()          <- debugPassText "starting optimize" "" (pure ())
  objects     <- optimize modul_ annotations canonical_
  return (Artifacts canonical_ annotations objects)


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
