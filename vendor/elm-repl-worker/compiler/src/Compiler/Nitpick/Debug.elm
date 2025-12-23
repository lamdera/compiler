{- MANUALLY FORMATTED -}
module Compiler.Nitpick.Debug exposing
  ( hasDebugUses
  )


import Compiler.AST.Optimized as Opt
import Extra.Type.List as MList
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- HAS DEBUG USES


hasDebugUses : Opt.LocalGraph -> Bool
hasDebugUses (Opt.LocalGraph _ graph _) =
  Map.any nodeHasDebug graph


nodeHasDebug : Opt.Node -> Bool
nodeHasDebug node =
  case node of
    Opt.Define expr _           -> hasDebug expr
    Opt.DefineTailFunc _ expr _ -> hasDebug expr
    Opt.Ctor _ _                -> False
    Opt.Enum _                  -> False
    Opt.Box                     -> False
    Opt.Link _                  -> False
    Opt.Cycle _ vs fs _         -> MList.any (hasDebug << Tuple.second) vs || MList.any defHasDebug fs
    Opt.Manager _               -> False
    Opt.Kernel _ _              -> False
    Opt.PortIncoming expr _     -> hasDebug expr
    Opt.PortOutgoing expr _     -> hasDebug expr


hasDebug : Opt.Expr -> Bool
hasDebug expression =
  case expression of
    Opt.CBool _          -> False
    Opt.Chr _            -> False
    Opt.Str _            -> False
    Opt.CInt _           -> False
    Opt.CFloat _         -> False
    Opt.VarLocal _       -> False
    Opt.VarGlobal _      -> False
    Opt.VarEnum _ _      -> False
    Opt.VarBox _         -> False
    Opt.VarCycle _ _     -> False
    Opt.VarDebug _ _ _ _ -> True
    Opt.VarKernel _ _    -> False
    Opt.CList exprs      -> MList.any hasDebug exprs
    Opt.Function _ expr  -> hasDebug expr
    Opt.Call e es        -> hasDebug e || MList.any hasDebug es
    Opt.TailCall _ args  -> MList.any (hasDebug << Tuple.second) args
    Opt.If conds finally -> MList.any (\(c,e) -> hasDebug c || hasDebug e) conds || hasDebug finally
    Opt.Let def body     -> defHasDebug def || hasDebug body
    Opt.Destruct _ expr  -> hasDebug expr
    Opt.Case _ _ d jumps -> deciderHasDebug d || MList.any (hasDebug << Tuple.second) jumps
    Opt.Accessor _       -> False
    Opt.Access r _       -> hasDebug r
    Opt.Update r fs      -> hasDebug r || Map.any hasDebug fs
    Opt.Record fs        -> Map.any hasDebug fs
    Opt.Unit             -> False
    Opt.Tuple a b c      -> hasDebug a || hasDebug b || MMaybe.maybe False hasDebug c
    Opt.Shader _ _ _     -> False


defHasDebug : Opt.Def -> Bool
defHasDebug def =
  case def of
    Opt.Def _ expr       -> hasDebug expr
    Opt.TailDef _ _ expr -> hasDebug expr


deciderHasDebug : Opt.Decider Opt.Choice -> Bool
deciderHasDebug decider =
  case decider of
    Opt.Leaf (Opt.Inline expr)  -> hasDebug expr
    Opt.Leaf (Opt.Jump _)       -> False
    Opt.Chain _ success failure -> deciderHasDebug success || deciderHasDebug failure
    Opt.FanOut _ tests fallback -> MList.any (deciderHasDebug << Tuple.second) tests || deciderHasDebug fallback



-- TODO: FIND GLOBALLY UNUSED DEFINITIONS?
-- TODO: FIND PACKAGE USAGE STATS? (e.g. elm/core = 142, author/project = 2, etc.)
