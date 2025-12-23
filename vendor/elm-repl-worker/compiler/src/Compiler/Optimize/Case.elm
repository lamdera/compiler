{- MANUALLY FORMATTED -}
module Compiler.Optimize.Case exposing
  ( optimize
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Name as Name
import Compiler.Optimize.DecisionTree as DT
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- OPTIMIZE A CASE EXPRESSION


optimize : Name.Name -> Name.Name -> TList (Can.Pattern, Opt.Expr) -> Opt.Expr
optimize temp root optBranches =
  let
    (patterns, indexedBranches) =
      MList.unzip (MList.zipWith indexify (MList.range 0 <| MList.length optBranches - 1) optBranches)

    decider = treeToDecider (DT.compile patterns)
    targetCounts = countTargets decider

    (choices, maybeJumps) =
      MList.unzip (MList.map (createChoices targetCounts) indexedBranches)
  in
  Opt.Case temp root
    (insertChoices (Map.fromList choices) decider)
    (MMaybe.catMaybes maybeJumps)


indexify : Int -> (a,b) -> ((a,Int), (Int,b))
indexify index (pattern, branch) =
  ( (pattern, index)
  , (index, branch)
  )



-- TREE TO DECIDER
--
-- Decision trees may have some redundancies, so we convert them to a Decider
-- which has special constructs to avoid code duplication when possible.


treeToDecider : DT.DecisionTree -> Opt.Decider Int
treeToDecider tree =
  case tree of
    DT.Match target ->
      Opt.Leaf target

    -- zero options
    DT.Decision _ [] Nothing ->
      Debug.todo "compiler bug, somehow created an empty decision tree"

    -- one option
    DT.Decision _ [(_, subTree)] Nothing ->
      treeToDecider subTree

    DT.Decision _ [] (Just subTree) ->
      treeToDecider subTree

    -- two options
    DT.Decision path [(test, successTree)] (Just failureTree) ->
      toChain path test successTree failureTree

    DT.Decision path [(test, successTree), (_, failureTree)] Nothing ->
      toChain path test successTree failureTree

    -- many options
    DT.Decision path edges Nothing ->
      let
        (necessaryTests, fallback) =
          (MList.init edges, Tuple.second (MList.last edges))
      in
        Opt.FanOut
          path
          (MList.map (Tuple.mapSecond treeToDecider) necessaryTests)
          (treeToDecider fallback)

    DT.Decision path edges (Just fallback) ->
      Opt.FanOut path (MList.map (Tuple.mapSecond treeToDecider) edges) (treeToDecider fallback)


toChain : DT.Path -> DT.Test -> DT.DecisionTree -> DT.DecisionTree -> Opt.Decider Int
toChain path test successTree failureTree =
  let
    failure =
      treeToDecider failureTree
  in
    let success_ = treeToDecider successTree
        otherwise () = Opt.Chain [(path, test)] success_ failure
    in
    case success_ of
      Opt.Chain testChain success subFailure -> if failure == subFailure then
        Opt.Chain ((path, test) :: testChain) success failure else otherwise ()

      _ ->
        otherwise ()



-- INSERT CHOICES
--
-- If a target appears exactly once in a Decider, the corresponding expression
-- can be inlined. Whether things are inlined or jumps is called a "choice".


countTargets : Opt.Decider Int -> Map.Map Int Int
countTargets decisionTree =
  case decisionTree of
    Opt.Leaf target ->
      Map.singleton target 1

    Opt.Chain _ success failure ->
      Map.unionWith (+) (countTargets success) (countTargets failure)

    Opt.FanOut _ tests fallback ->
      Map.unionsWith MList.foldl (+) (MList.map countTargets (fallback :: MList.map Tuple.second tests))


createChoices
  : Map.Map Int Int
  -> (Int, Opt.Expr)
  -> ( (Int, Opt.Choice), Maybe (Int, Opt.Expr) )
createChoices targetCounts (target, branch) =
  if Map.ex targetCounts target == 1 then
    ( (target, Opt.Inline branch)
    , Nothing
    )

  else
    ( (target, Opt.Jump target)
    , Just (target, branch)
    )


insertChoices
  : Map.Map Int Opt.Choice
  -> Opt.Decider Int
  -> Opt.Decider Opt.Choice
insertChoices choiceDict decider =
  let
    go =
      insertChoices choiceDict
  in
    case decider of
      Opt.Leaf target ->
        Opt.Leaf (Map.ex choiceDict target)

      Opt.Chain testChain success failure ->
        Opt.Chain testChain (go success) (go failure)

      Opt.FanOut path tests fallback ->
        Opt.FanOut path (MList.map (Tuple.mapSecond go) tests) (go fallback)
