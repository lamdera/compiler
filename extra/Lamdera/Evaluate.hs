{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Evaluate where

import qualified Data.Map as Map

import AST.Canonical
import Elm.Package
import qualified Reporting.Annotation as A
import qualified Elm.ModuleName as Module
import Lamdera
import Lamdera.Wire3.Helpers
import StandaloneInstances

import qualified Lamdera.Interfaces
import qualified Lamdera.Canonical
import Lamdera
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Lamdera.Compile

import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Data.Name
import qualified Data.Text as T
import qualified AST.Optimized as Opt

exec = do
  let project = "/Users/mario/dev/projects/elmx/test/scenario-interpreter"
  withCurrentDirectory project $ do

    canonical <- Lamdera.Canonical.loadSingleCanonical "src/Test/Basic.elm"
    objects <- Lamdera.Canonical.loadSingleObjects "src/Test/Basic.elm"

    -- objects
    --   & _l_nodes
    --   &

    let name = "suite"
    objects
      & Opt._l_nodes
      -- & Map.lookup name
      & Map.filterWithKey (\k _ ->
          case k of
            Opt.Global (Module.Canonical (Pkg.Name _ _) _) name_ ->
              name_ == name
        )
      & formatHaskellValue ("stuff")


    formatHaskellValue "objects Test.Basic" objects

    -- formatHaskellValue "canonical Test.Basic" canonical
    --
    -- case canonical & _decls & findDef "suite" of
    --   Just def -> do
    --     formatHaskellValue "suite def" def
    --     formatHaskellValue "suite:" $ run def Map.empty
    --
    --   Nothing ->
    --     putStrLn "no suite found"

    pure ()



-- Evaluate AST.Canonical expression. Note, it turns out some concepts are missing (i.e. normalisation of Bool values)
-- so it may well be that AST.Optimized is a better AST to use for the interpreter.


{-

  addOne x =
    x + 1

-}
exampleDef =
  Def (a ("addOne"))
    [(a (PVar "x"))]
    (a (Binop
          "+"
          (Module.Canonical (Name "elm" "core") "Basics")
          "add"
          (Forall (Map.fromList [("number", ())]) (TLambda (TVar "number") (TLambda (TVar "number") (TVar "number"))))
          (a (VarLocal "x"))
          (a (Int 1))))


test x =
  run exampleDef (Map.fromList [("x", Int x)])


run def args =
  case def of
    Def name pargs expr ->
      evaluate_ expr args


evaluate_ expr args =
  case dropAnnotation expr of
    Binop "+" _ _ _ left right ->
      case (evaluate_ left args, evaluate_ right args) of
        (Int l, Int r) -> Int (l + r)

    Binop "==" _ _ _ left right ->
      case (evaluate_ left args, evaluate_ right args) of
        (l, r) ->
          -- This is obviously wrong, but there is no raw Bool in Canonical!
          -- It's still just a union type from Basics. So this function becomes
          -- really tricky to implement at this level.
          Int 3444344434434434

    VarLocal name ->
      case Map.lookup name args of
        Just v -> v
        Nothing ->
          error "failed!"

    Int x -> Int x

    _ -> error $ "Not yet implemented! " ++ show (dropAnnotation expr)


dropAnnotation expr =
  case expr of
    A.At r n_ -> n_
