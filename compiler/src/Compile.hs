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
import qualified Reporting.Annotation
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

  let
    updateExpr :: Can.Expr -> Can.Expr
    updateExpr (Reporting.Annotation.At location expr) =
        (case expr of
            Can.VarLocal name ->
                Can.VarLocal name
            Can.VarTopLevel canonical name ->
                Can.VarTopLevel canonical name
            Can.VarKernel name name2 ->
                Can.VarKernel name name2
            Can.VarForeign canonical name annotation ->
                Can.VarForeign canonical name annotation
            Can.VarCtor ctorOpts canonical name zeroBased annotation ->
                Can.VarCtor ctorOpts canonical name zeroBased annotation
            Can.VarDebug canonical name annotation ->
                Can.VarDebug canonical name annotation
            Can.VarOperator name canonical name2 annotation ->
                Can.VarOperator name canonical name2 annotation
            Can.Chr string ->
                Can.Chr string
            Can.Str string ->
                Can.Str string
            Can.Int int ->
                Can.Int int
            Can.Float float ->
                Can.Float float
            Can.List exprs ->
                Can.List (fmap updateExpr exprs)
            Can.Negate expr ->
                Can.Negate (updateExpr expr)
            Can.Binop name canonical name2 annotation expr expr2 ->
                Can.Binop name canonical name2 annotation (updateExpr expr) (updateExpr expr2)
            Can.Lambda patterns expr ->
                Can.Lambda patterns (updateExpr expr)
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (ModuleName.Canonical package "Element")
                        "el"
                        annotation
                    )
                )
                (firstParam : rest) ->
                let
                    backgroundColorAnnotation =
                        0

                    newAttributes =
                        Reporting.Annotation.At
                            location
                            (Can.List
                                [ Reporting.Annotation.At
                                    location
                                    (Can.Call
                                        (Reporting.Annotation.At
                                            location
                                            (Can.VarForeign
                                                (ModuleName.Canonical package "Element.Background")
                                                "color"
                                                annotation
                                            )
                                        )
                                        []
                                    )
                                ]
                            )

--                     finalAttributes =
--                         Reporting.Annotation.At
--                             location
--                             (Can.BinOps
--
--                                 newAttributes
--                             )

                in
                Can.Call
                    (Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (ModuleName.Canonical package "Element")
                            (dt "Note" "el")
                            annotation)
                        )
                    (fmap updateExpr (firstParam : rest))
            Can.Call expr exprs ->
                Can.Call (updateExpr expr) (fmap updateExpr exprs)
            Can.If exprs expr ->
                Can.If
                    (fmap (\(first, second) -> (updateExpr first, updateExpr second)) exprs)
                    (updateExpr expr)
            Can.Let def expr ->
                Can.Let def (updateExpr expr)
            Can.LetRec defs expr ->
                Can.LetRec defs (updateExpr expr)
            Can.LetDestruct pattern expr expr2 ->
                Can.LetDestruct pattern (updateExpr expr) (updateExpr expr2)
            Can.Case expr caseBranches ->
                Can.Case
                    (updateExpr expr)
                    (fmap
                        (\(Can.CaseBranch pattern caseExpr) ->
                            Can.CaseBranch pattern (updateExpr caseExpr)
                        )
                        caseBranches
                    )
            Can.Accessor name ->
                Can.Accessor name
            Can.Access expr name ->
                Can.Access (updateExpr expr) name
            Can.Update name expr fieldUpdates ->
                Can.Update name (updateExpr expr) fieldUpdates
            Can.Record fields ->
                Can.Record fields
            Can.Unit ->
                Can.Unit
            Can.Tuple expr expr2 maybeExpr ->
                Can.Tuple (updateExpr expr) (updateExpr expr2) (fmap updateExpr maybeExpr)
            Can.Shader shaderSource shaderTypes ->
                Can.Shader shaderSource shaderTypes
        )
        & Reporting.Annotation.At location

    updateDefs :: Can.Def -> Can.Def
    updateDefs def =
        case def of
            Can.Def name patterns expr ->
                Can.Def name patterns (updateExpr expr)

            Can.TypedDef name freeVars patterns expr type_ ->
                Can.TypedDef name freeVars patterns (updateExpr expr) type_


    updateDecls :: Can.Decls -> Can.Decls
    updateDecls decls =
      -- error "todo!"
        case decls of
            Can.Declare def nextDecl ->
                Can.Declare (updateDefs def) (updateDecls nextDecl)

            Can.DeclareRec def remainingDefs nextDecl ->
                Can.DeclareRec
                    (updateDefs def)
                    (map updateDefs remainingDefs)
                    (updateDecls nextDecl)
                
            Can.SaveTheEnvironment ->
                Can.SaveTheEnvironment

    canonical3 :: Can.Module
    canonical3 =
      (Can._decls canonical2)
        & updateDecls
        & (\newDecls -> canonical2 { Can._decls = newDecls })


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
  annotations <- typeCheck modul_ canonical3
  -- ()          <- debugPassText "starting nitpick" moduleName (pure ())
  ()          <- nitpick canonical3




  -- ()          <- debugPassText "starting optimize" moduleName (pure ())
  objects     <- optimize modul_ annotations canonical3

  return (Artifacts canonical3 annotations objects)


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
