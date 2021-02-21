{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lamdera.Evaluate.Optimized where

import qualified Data.List as List
import Data.Int (Int64)
import Text.Read (readMaybe)

import AST.Optimized
import qualified Elm.ModuleName as Module
import qualified Data.Name
import qualified Data.Utf8 as Utf8

import Elm.Package
import qualified Reporting.Annotation as A
import qualified Elm.ModuleName as Module
import Lamdera
import Lamdera.Wire3.Helpers
import StandaloneInstances

import Lamdera
import Data.Map ((!))
import Data.Set (fromList)
import qualified Data.Map as Map
import qualified Lamdera.Compile

import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Data.Name
import qualified Data.Text as T
import qualified AST.Optimized as Opt

import qualified Elm.Float

{-

  addOne x =
    x + 1

-}
exampleDef =
  -- Def (a ("addOne"))
  --   [(a (PVar "x"))]
  --   (a (Binop
  --         "+"
  --         (Module.Canonical (Name "elm" "core") "Basics")
  --         "add"
  --         (Forall (Map.fromList [("number", ())]) (TLambda (TVar "number") (TLambda (TVar "number") (TVar "number"))))
  --         (a (VarLocal "x"))
  --         (a (Int 1))))
  Define
      (Function ["x"] (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [VarLocal "x", Int 1]))
      (fromList [Global (Module.Canonical (Name "elm" "core") "Basics") "add"])


-- test x =
--   run exampleDef (Map.fromList [("x", Int x)])


run def args nodes =
  case def of
    Define expr funcDeps ->
      evaluate_ expr args nodes


-- call def args =
--   evaluate_ (Call def args)



evaluate_ expr locals globals =
  -- case debugPass "expr" (expr, locals) expr of
  -- debugHaskell (show_ expr) $
   case expr of
    -- (Call (VarGlobal (Global (Module.Canonical (Name "author" "project") "Test.Basic") "addOne")) args_) ->
    --     let
    --       inlined = (Function ["x"] (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [VarLocal "x", Int 1]))
    --     in
    --     evaluate_ (Call inlined args_) (locals) globals

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [arg1, arg2]) ->
      let
        pFloat :: Elm.Float.Float -> Maybe Double
        pFloat = readMaybe . Utf8.toChars
      in
      case [evaluate_ arg1 locals globals, evaluate_ arg2 locals globals] of
        [Int a, Int b] -> Int $ a + b
        [Float a, Float b] -> do
          case (pFloat a, pFloat b) of
            (Just a_, Just b_) -> Float $ Utf8.fromChars $ show $ a_ + b_
            _ -> error $ "impossible failure to parse floats: " <> show a <> ", " <> show b
        [Int a, Float b] -> do
          case pFloat b of
            Just b_ -> Float $ Utf8.fromChars $ show $ (fromIntegral a) + b_
            _ -> error $ "impossible failure to parse float: " <> show b
        [Float a, Int b] -> do
          case pFloat a of
            Just a_ -> Float $ Utf8.fromChars $ show $ a_ + (fromIntegral b)
            _ -> error $ "impossible failure to parse float: " <> show a

        evaled -> error $ "unhandled Basics.add: " <> show evaled

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "eq")) [arg1, arg2]) ->
      case [evaluate_ arg1 locals globals, evaluate_ arg2 locals globals] of
        [a, b] -> Bool $ a == b

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "append")) [arg1, arg2]) ->
      case [evaluate_ arg1 locals globals, evaluate_ arg2 locals globals] of
        [Str a, Str b] -> Str $ Utf8.fromChars $ Utf8.toChars a <> Utf8.toChars b

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "apL")) [arg1, arg2]) ->
      let
        left = evaluate_ arg1 locals globals
        right = evaluate_ arg2 locals globals
      in
      -- debugHaskellPass "apL" ([arg1, arg2], left, right) $
        evaluate_ (Call left [right]) locals globals


    (Call (VarGlobal global) args_) ->
      let inlined = getDef global globals
      in
      -- debugHaskell (show_ (inlined, args_)) $
        evaluate_ (Call inlined args_) locals globals

    (Call expr args) ->
      case expr of
        (Function pnames def) ->
          if length pnames /= length args then
            -- This means we're currying
            let (locals_, pnames_) = zipRem pnames args
            in
            (Function pnames_ (applyVarLocals (locals_ & Map.fromList) def))
            -- error $ "lengths do not match! " <> (T.unpack $ hindentFormatValue (pnames, args))
          else
            evaluate_ def (zip pnames args & Map.fromList) globals

        (Call expr args) ->
          -- We have all the args we need to reduce the full expression
          evaluate_ (Call expr args) locals globals

        _ ->
          error $ "call(expr) not handled: \n" <> show expr

    VarLocal n ->
      case Map.lookup n locals of
        Just v -> evaluate_ v Map.empty globals
        Nothing ->
          -- Could be a curried function!
          expr
          -- error $ "'VarLocal "<> show n <> "' not found in locals: " <> show locals

    Int n -> expr
    Float n -> expr
    Str s -> expr
    Bool b -> expr

    List exprs -> exprs & fmap (\expr -> evaluate_ expr locals globals) & List

    If conditionBranches elseBranch ->
      conditionBranches
        & List.find (\(condition, def) ->
          -- debugPass "evaluating on" (condition, locals) $
            evaluate_ condition locals globals == Bool True
        )
        & (\defM ->
          case defM of
            Just (_, def) ->
              -- debugPass "evaluating on" def $
                evaluate_ def locals globals
            Nothing ->
              -- debugPass "passing to else" () $
                evaluate_ elseBranch locals globals
        )

    _ ->
      error $ "evaluate unimplemented:\n" <> (T.unpack $ hindentFormatValue expr)



applyVarLocals locals expr =
  case expr of
    VarLocal n ->
      case Map.lookup n locals of
        Just v -> v
        Nothing ->
          -- Could be a curried function!
          expr

    VarGlobal _ -> expr
    Str _ -> expr
    Int _ -> expr

    (Call expr args) ->
      Call (applyVarLocals locals expr) (fmap (applyVarLocals locals) args)

    If conditionBranches elseBranch ->
      If
        (conditionBranches & fmap (\(condition,branch) -> (applyVarLocals locals condition, applyVarLocals locals branch)))
        (elseBranch & applyVarLocals locals )

    _ ->
      error $ "applyVarLocals missing coverage: " <> show expr


getDef global globals =
  -- @TODO currently actually local file objects, but need to expand to full project
    case Map.lookup global globals of
      Just node ->
        case node of
          Define expr funcDeps ->
            expr

          _ ->
            error $ "unimplemented getDef node: " <> show node

      Nothing ->
        error $ "Global not found in globals:\n"<> show global <> "\n\n "-- <> show globals


zipRem l1 l2 =
  let zipped = zip l1 l2
  in
  (zipped, drop (length zipped) l1)
