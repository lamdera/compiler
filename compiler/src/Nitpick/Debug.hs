{-# LANGUAGE OverloadedStrings #-}

module Nitpick.Debug
  ( hasDebugUses
  )
  where


import qualified Data.Map.Utils as Map
import StandaloneInstances

import qualified AST.Optimized as Opt
import Lamdera


-- HAS DEBUG USES


hasDebugUses :: Bool -> Opt.LocalGraph -> Bool
hasDebugUses allowDebugLog (Opt.LocalGraph _ graph _) =
  Map.any (nodeHasDebug allowDebugLog) graph


nodeHasDebug :: Bool -> Opt.Node -> Bool
nodeHasDebug allowDebugLog node =
  case node of
    Opt.Define expr _           -> hasDebug allowDebugLog expr
    Opt.DefineTailFunc _ expr _ -> hasDebug allowDebugLog expr
    Opt.Ctor _ _                -> False
    Opt.Enum _                  -> False
    Opt.Box                     -> False
    Opt.Link _                  -> False
    Opt.Cycle _ vs fs _         -> any ((hasDebug allowDebugLog) . snd) vs || any (defHasDebug allowDebugLog) fs
    Opt.Manager _               -> False
    Opt.Kernel _ _              -> False
    Opt.PortIncoming expr _     -> hasDebug allowDebugLog expr
    Opt.PortOutgoing expr _     -> hasDebug allowDebugLog expr


hasDebug :: Bool -> Opt.Expr -> Bool
hasDebug allowDebugLog expression =
  case expression of
    Opt.Bool _           -> False
    Opt.Chr _            -> False
    Opt.Str _            -> False
    Opt.Int _            -> False
    Opt.Float _          -> False
    Opt.VarLocal _       -> False
    Opt.VarGlobal _      -> False
    Opt.VarEnum _ _      -> False
    Opt.VarBox _         -> False
    Opt.VarCycle _ _     -> False
    Opt.VarDebug name _ _ _ -> Lamdera.alternativeImplementationWhen allowDebugLog (name /= "log") True
    Opt.VarKernel _ _    -> False
    Opt.List exprs       -> any (hasDebug allowDebugLog) exprs
    Opt.Function _ expr  -> hasDebug allowDebugLog expr
    Opt.Call e es        -> hasDebug allowDebugLog e || any (hasDebug allowDebugLog) es
    Opt.TailCall _ args  -> any ((hasDebug allowDebugLog) . snd) args
    Opt.If conds finally -> any (\(c,e) -> hasDebug allowDebugLog c || hasDebug allowDebugLog e) conds || hasDebug allowDebugLog finally
    Opt.Let def body     -> defHasDebug allowDebugLog def || hasDebug allowDebugLog body
    Opt.Destruct _ expr  -> hasDebug allowDebugLog expr
    Opt.Case _ _ d jumps -> deciderHasDebug allowDebugLog d || any ((hasDebug allowDebugLog) . snd) jumps
    Opt.Accessor _       -> False
    Opt.Access r _       -> hasDebug allowDebugLog r
    Opt.Update r fs      -> hasDebug allowDebugLog r || any (hasDebug allowDebugLog) fs
    Opt.Record fs        -> any (hasDebug allowDebugLog) fs
    Opt.Unit             -> False
    Opt.Tuple a b c      -> hasDebug allowDebugLog a || hasDebug allowDebugLog b || maybe False (hasDebug allowDebugLog) c
    Opt.Shader _ _ _     -> False


defHasDebug :: Bool -> Opt.Def -> Bool
defHasDebug allowDebugLog def =
  case def of
    Opt.Def _ expr       -> hasDebug allowDebugLog expr
    Opt.TailDef _ _ expr -> hasDebug allowDebugLog expr


deciderHasDebug :: Bool -> Opt.Decider Opt.Choice -> Bool
deciderHasDebug allowDebugLog decider =
  case decider of
    Opt.Leaf (Opt.Inline expr)  -> hasDebug allowDebugLog expr
    Opt.Leaf (Opt.Jump _)       -> False
    Opt.Chain _ success failure -> deciderHasDebug allowDebugLog success || deciderHasDebug allowDebugLog failure
    Opt.FanOut _ tests fallback -> any ((deciderHasDebug allowDebugLog) . snd) tests || deciderHasDebug allowDebugLog fallback



-- TODO: FIND GLOBALLY UNUSED DEFINITIONS?
-- TODO: FIND PACKAGE USAGE STATS? (e.g. elm/core = 142, author/project = 2, etc.)
