{- MANUALLY FORMATTED -}
module Compiler.Optimize.DecisionTree exposing
  ( DecisionTree(..)
  , compile
  , Path(..), bPath
  , Test(..), bTest
  )


{- To learn more about how this works, definitely read through:

    "When Do Match-Compilation Heuristics Matter?"

by Kevin Scott and Norman Ramsey. The rough idea is that we start with a simple
list of patterns and expressions, and then turn that into a "decision tree"
that requires as few tests as possible to make it to a leaf. Read the paper, it
explains this extraordinarily well! We are currently using the same heuristics
as SML/NJ to get nice trees.
-}

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.String as ES
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set



-- COMPILE CASES


{-| Users of this module will mainly interact with this function. It takes
some normal branches and gives out a decision tree that has "labels" at all
the leafs and a dictionary that maps these "labels" to the code that should
run.

If 2 or more leaves point to the same label, we need to do some tricks in JS to
make that work nicely. When is JS getting goto?! ;) That is outside the scope
of this module though.
-}
compile : TList (Can.Pattern, Int) -> DecisionTree
compile rawBranches =
  let
    format (pattern, index) =
      Branch index [(Empty, pattern)]
  in
    toDecisionTree (MList.map format rawBranches)



-- DECISION TREES


type DecisionTree
  = Match Int
  | Decision
      {- path -} Path
      {- edges -} (TList (Test, DecisionTree))
      {- default -} (Maybe DecisionTree)


type Test
  = IsCtor ModuleName.Canonical Name.Name Index.ZeroBased Int Can.CtorOpts
  | IsCons
  | IsNil
  | IsTuple
  | IsInt Int
  | IsChr ES.TString
  | IsStr ES.TString
  | IsBool Bool


toTestComparable : Test -> String
toTestComparable test =
  case test of
    IsCtor home name index numAlts opts ->
      "ctor:" ++ ModuleName.toString home
      ++ ":" ++ name
      ++ ":" ++ String.fromInt (Index.toMachine index)
      ++ ":" ++ String.fromInt numAlts
      ++ ":" ++ Can.ctorOptsToString opts

    IsCons ->
      "cons"

    IsNil ->
      "nil"

    IsTuple ->
      "tuple"

    IsInt int ->
      "int:" ++ String.fromInt int

    IsChr chr ->
      "chr:" ++ chr

    IsStr str ->
      "str:" ++ str

    IsBool bool ->
      "bool:" ++ if bool then "True" else "False"


type Path
  = Index Index.ZeroBased Path
  | Unbox Path
  | Empty



-- ACTUALLY BUILD DECISION TREES


type Branch =
  Branch
    {- goal -} Int
    {- patterns -} (TList (Path, Can.Pattern))


getPatterns (Branch _ patterns) = patterns


toDecisionTree : TList Branch -> DecisionTree
toDecisionTree rawBranches =
  let
    branches =
      MList.map flattenPatterns rawBranches
  in
  case checkForMatch branches of
    Just goal ->
      Match goal

    Nothing ->
      let
        path =
          pickPath branches

        (edges, fallback) =
          gatherEdges branches path

        decisionEdges =
          MList.map (Tuple.mapSecond toDecisionTree) edges
      in
      case (decisionEdges, fallback) of
        ([(_, decisionTree)], []) ->
          decisionTree

        (_, []) ->
            Decision path decisionEdges Nothing

        ([], _ :: _) ->
          toDecisionTree fallback

        _ ->
            Decision path decisionEdges (Just (toDecisionTree fallback))


isComplete : TList Test -> Bool
isComplete tests =
  case MList.head tests of
    IsCtor _ _ _ numAlts _ ->
      numAlts == MList.length tests

    IsCons ->
      MList.length tests == 2

    IsNil ->
      MList.length tests == 2

    IsTuple ->
      True

    IsChr _ ->
      False

    IsStr _ ->
      False

    IsInt _ ->
      False

    IsBool _ ->
      MList.length tests == 2



-- FLATTEN PATTERNS


{-| Flatten type aliases and use the VariantDict to figure out when a tag is
the only variant so we can skip doing any tests on it.
-}
flattenPatterns : Branch -> Branch
flattenPatterns (Branch goal pathPatterns) =
  Branch goal (MList.foldr flatten [] pathPatterns)


flatten : (Path, Can.Pattern) -> TList (Path, Can.Pattern) -> TList (Path, Can.Pattern)
flatten ((path, A.At region pattern) as pathPattern) otherPathPatterns =
  case pattern of
    Can.PVar _ ->
      pathPattern :: otherPathPatterns

    Can.PAnything ->
      pathPattern :: otherPathPatterns

    Can.PCtor _ _ (Can.Union _ _ numAlts _) _ _ ctorArgs ->
      if numAlts == 1 then
        case MList.map dearg ctorArgs of
          [arg] ->
            flatten (Unbox path, arg) otherPathPatterns

          args ->
            MList.foldr flatten otherPathPatterns (subPositions path args)
      else
        pathPattern :: otherPathPatterns

    Can.PTuple a b maybeC ->
      flatten (Index Index.first path, a) <|
      flatten (Index Index.second path, b) <|
        case maybeC of
          Nothing ->
            otherPathPatterns

          Just c ->
            flatten (Index Index.third path, c) otherPathPatterns

    Can.PUnit ->
      otherPathPatterns

    Can.PAlias realPattern alias ->
      flatten (path, realPattern) <|
        (path, A.At region (Can.PVar alias)) :: otherPathPatterns

    Can.PRecord _ ->
      pathPattern :: otherPathPatterns

    Can.PList _ ->
      pathPattern :: otherPathPatterns

    Can.PCons _ _ ->
      pathPattern :: otherPathPatterns

    Can.PChr _ ->
      pathPattern :: otherPathPatterns

    Can.PStr _ ->
      pathPattern :: otherPathPatterns

    Can.PInt _ ->
      pathPattern :: otherPathPatterns

    Can.PBool _ _ ->
      pathPattern :: otherPathPatterns


subPositions : Path -> TList Can.Pattern -> TList (Path, Can.Pattern)
subPositions path patterns =
  Index.indexedMap (\index pattern -> (Index index path, pattern)) patterns


dearg : Can.PatternCtorArg -> Can.Pattern
dearg (Can.PatternCtorArg _ _ pattern) =
  pattern



-- SUCCESSFULLY MATCH


{-| If the first branch has no more "decision points" we can finally take that
path. If that is the case we give the resulting label and a mapping from free
variables to "how to get their value". So a pattern like (Just (x,_)) will give
us something like ("x" => value.0.0)
-}
checkForMatch : TList Branch -> Maybe Int
checkForMatch branches =
  case branches of
    Branch goal patterns :: _ -> if MList.all (not << needsTests << Tuple.second) patterns then
      Just goal else Nothing

    _ ->
      Nothing



-- GATHER OUTGOING EDGES


gatherEdges : TList Branch -> Path -> (TList (Test, TList Branch), TList Branch)
gatherEdges branches path =
  let
    relevantTests =
      testsAtPath path branches

    allEdges =
      MList.map (edgesFor path branches) relevantTests

    fallbacks =
      if isComplete relevantTests then
        []
      else
        MList.filter (isIrrelevantTo path) branches
  in
  ( allEdges, fallbacks )



-- FIND RELEVANT TESTS


testsAtPath : Path -> TList Branch -> TList Test
testsAtPath selectedPath branches =
  let
    allTests =
      MMaybe.mapMaybe (testAtPath selectedPath) branches

    skipVisited test ((uniqueTests, visitedTests) as curr) =
      let
        testComparable =
          toTestComparable test
      in
      if Set.member testComparable visitedTests then
        curr
      else
        ( test :: uniqueTests
        , Set.insert testComparable visitedTests
        )
  in
  Tuple.first (MList.foldr skipVisited ([], Set.empty) allTests)


testAtPath : Path -> Branch -> Maybe Test
testAtPath selectedPath (Branch _ pathPatterns) =
  MMaybe.bind (MList.lookup selectedPath pathPatterns) <| \(A.At _ pattern) ->
    case pattern of
      Can.PCtor home _ (Can.Union _ _ numAlts opts) name index _ ->
        Just (IsCtor home name index numAlts opts)

      Can.PList ps ->
        Just <| case ps of [] -> IsNil
                           _  -> IsCons

      Can.PCons _ _ ->
        Just IsCons

      Can.PTuple _ _ _ ->
        Just IsTuple

      Can.PUnit ->
        Just IsTuple

      Can.PVar _ ->
        Nothing

      Can.PAnything ->
        Nothing

      Can.PInt int ->
        Just (IsInt int)

      Can.PStr str ->
        Just (IsStr str)

      Can.PChr chr ->
        Just (IsChr chr)

      Can.PBool _ bool ->
        Just (IsBool bool)

      Can.PRecord _ ->
        Nothing

      Can.PAlias _ _ ->
        Debug.todo "aliases should never reach 'testAtPath' function"



-- BUILD EDGES


edgesFor : Path -> TList Branch -> Test -> (Test, TList Branch)
edgesFor path branches test =
  ( test
  , MMaybe.mapMaybe (toRelevantBranch test path) branches
  )


toRelevantBranch : Test -> Path -> Branch -> Maybe Branch
toRelevantBranch test path ((Branch goal pathPatterns) as branch) =
  case extract path pathPatterns of
    Found start (A.At region pattern) end ->
      case pattern of
        Can.PCtor _ _ (Can.Union _ _ numAlts _) name _ ctorArgs ->
          case test of
            IsCtor _ testName _ _ _ -> if name == testName then
              Just <| Branch goal <|
                let
                  args = MList.map dearg ctorArgs
                  otherwise () = start ++ subPositions path args ++ end
                in
                case args of
                  [arg] -> if numAlts == 1 then
                    start ++ (Unbox path, arg) :: end else otherwise ()

                  _ ->
                    otherwise ()
              else Nothing

            _ ->
              Nothing

        Can.PList [] ->
          case test of
            IsNil ->
              Just (Branch goal (start ++ end))

            _ ->
              Nothing

        Can.PList (hd::tl) ->
          case test of
            IsCons ->
              let tl_ = A.At region (Can.PList tl) in
              Just (Branch goal (start ++ subPositions path [ hd, tl_ ] ++ end))

            _ ->
              Nothing

        Can.PCons hd tl ->
          case test of
            IsCons ->
              Just (Branch goal (start ++ subPositions path [hd,tl] ++ end))

            _ ->
              Nothing

        Can.PChr chr ->
          case test of
            IsChr testChr -> if chr == testChr then
              Just (Branch goal (start ++ end)) else Nothing
            _ ->
              Nothing

        Can.PStr str ->
          case test of
            IsStr testStr -> if str == testStr then
              Just (Branch goal (start ++ end)) else Nothing

            _ ->
              Nothing

        Can.PInt int ->
          case test of
            IsInt testInt -> if int == testInt then
              Just (Branch goal (start ++ end)) else Nothing

            _ ->
              Nothing

        Can.PBool _ bool ->
          case test of
            IsBool testBool -> if bool == testBool then
              Just (Branch goal (start ++ end)) else Nothing

            _ ->
              Nothing

        Can.PUnit ->
          Just (Branch goal (start ++ end))

        Can.PTuple a b maybeC ->
          Just (Branch goal (start ++ subPositions path (a :: b :: MMaybe.maybeToList maybeC) ++ end))

        Can.PVar _ ->
          Just branch

        Can.PAnything ->
          Just branch

        Can.PRecord _ ->
          Just branch

        Can.PAlias _ _ ->
          Just branch

    NotFound ->
      Just branch


type Extract
  = NotFound
  | Found (TList (Path, Can.Pattern)) Can.Pattern (TList (Path, Can.Pattern))


extract : Path -> TList (Path, Can.Pattern) -> Extract
extract selectedPath pathPatterns =
  case pathPatterns of
    [] ->
      NotFound

    ((path, pattern) as first) :: rest ->
      if path == selectedPath then
        Found [] pattern rest

      else
        case extract selectedPath rest of
          NotFound ->
            NotFound

          Found start foundPattern end ->
            Found (first :: start) foundPattern end



-- FIND IRRELEVANT BRANCHES


isIrrelevantTo : Path -> Branch -> Bool
isIrrelevantTo selectedPath (Branch _ pathPatterns) =
  case MList.lookup selectedPath pathPatterns of
    Nothing ->
      True

    Just pattern ->
      not (needsTests pattern)


needsTests : Can.Pattern -> Bool
needsTests (A.At _ pattern) =
  case pattern of
    Can.PVar _            -> False
    Can.PAnything         -> False
    Can.PRecord _         -> False
    Can.PCtor _ _ _ _ _ _ -> True
    Can.PList _           -> True
    Can.PCons _ _         -> True
    Can.PUnit             -> True
    Can.PTuple _ _ _      -> True
    Can.PChr _            -> True
    Can.PStr _            -> True
    Can.PInt _            -> True
    Can.PBool _ _         -> True
    Can.PAlias _ _ ->
      Debug.todo "aliases should never reach 'isIrrelevantTo' function"




-- PICK A PATH


pickPath : TList Branch -> Path
pickPath branches =
  let
    allPaths =
      MMaybe.mapMaybe isChoicePath (MList.concatMap getPatterns branches)
  in
    case bests (addWeights (smallDefaults branches) allPaths) of
      [path] ->
        path

      tiedPaths ->
        MList.head (bests (addWeights (smallBranchingFactor branches) tiedPaths))


isChoicePath : (Path, Can.Pattern) -> Maybe Path
isChoicePath (path, pattern) =
  if needsTests pattern then
    Just path
  else
    Nothing


addWeights : (Path -> Int) -> TList Path -> TList (Path, Int)
addWeights toWeight paths =
  MList.map (\path -> (path, toWeight path)) paths


bests : TList (Path, Int) -> TList Path
bests allPaths =
  case allPaths of
    [] ->
      Debug.todo "Cannot choose the best of zero paths. This should never happen."

    (headPath, headWeight) :: weightedPaths ->
      let
        gatherMinimum ((minWeight, paths) as acc) (path, weight) =
          if weight == minWeight then
            (minWeight, path :: paths)

          else if weight < minWeight then
            (weight, [path])

          else
            acc
      in
        Tuple.second (MList.foldl gatherMinimum (headWeight, [headPath]) weightedPaths)



-- PATH PICKING HEURISTICS


smallDefaults : TList Branch -> Path -> Int
smallDefaults branches path =
  MList.length (MList.filter (isIrrelevantTo path) branches)


smallBranchingFactor : TList Branch -> Path -> Int
smallBranchingFactor branches path =
  let
    (edges, fallback) =
      gatherEdges branches path
  in
    MList.length edges + (if MList.null fallback then 0 else 1)



-- BINARY


bTest : B.Binary Test
bTest =
  B.custom "problem getting DecisionTree.Test binary"
    (\p0 p1 p2 p3 p4 p5 p6 p7 test ->
      case test of
        IsCtor a b c d e -> p0 a b c d e
        IsCons           -> p1
        IsNil            -> p2
        IsTuple          -> p3
        IsChr a          -> p4 a
        IsStr a          -> p5 a
        IsInt a          -> p6 a
        IsBool a         -> p7 a
    )
    |> B.var5 0 IsCtor ModuleName.bCanonical Name.bName Index.bZeroBased B.bWord64 Can.bCtorOpts
    |> B.var0 1 IsCons
    |> B.var0 2 IsNil
    |> B.var0 3 IsTuple
    |> B.var1 4 IsChr ES.bTString
    |> B.var1 5 IsStr ES.bTString
    |> B.var1 6 IsInt B.bWord64
    |> B.var1 7 IsBool B.bBool
    |> B.finish


bPath : B.Binary Path
bPath =
  B.custom "problem getting DecisionTree.Path binary"
    (\p0 p1 p2 path ->
      case path of
        Index a b -> p0 a b
        Unbox a   -> p1 a
        Empty     -> p2
    )
    |> B.var2 0 Index Index.bZeroBased (B.lazy (\() -> bPath))
    |> B.var1 1 Unbox (B.lazy (\() -> bPath))
    |> B.var0 2 Empty
    |> B.finish
