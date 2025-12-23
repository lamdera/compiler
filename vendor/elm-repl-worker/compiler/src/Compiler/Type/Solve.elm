{- MANUALLY FORMATTED -}
module Compiler.Type.Solve exposing
  ( run
  , init
  )


import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Type as Error
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Type.Error as ET
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type
import Compiler.Type.Unify as Unify
import Compiler.Type.UnionFind as UF
import Extra.System.IO.Pure as IO
import Extra.System.MVector as MVector
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Tuple as MTuple


-- IO


type alias IO a =
  IO.IO IOState a


type alias IOState =
  ( -- types
    Type.State
  , -- pools
    MVector.State (TList Type.Variable)
  )


init : IOState
init =
  ( -- types
    Type.init
  , -- pools
    MVector.init
  )


liftP : MVector.IO (TList Type.Variable) a -> IO a
liftP = IO.liftS (\( _, x ) -> x) (\x ( a, _ ) -> ( a, x ))



-- RUN SOLVER


run : Type.Constraint -> IO (Either (NE.TList Error.Error) (Map.Map Name.Name Can.Annotation))
run constraint =
  IO.bind (liftP <| MVector.replicate 8 []) <| \pools ->

  IO.bind
    (solve Map.empty Type.outermostRank pools emptyState constraint) <| \(State env _ errors) ->

  case errors of
    [] ->
      IO.fmap Right <| IO.traverseMap Type.toAnnotation env

    e::es ->
      IO.return <| Left (NE.CList e es)


emptyState : State
emptyState =
  State Map.empty (Type.nextMark Type.noMark) []



-- SOLVER


type alias Env =
  Map.Map Name.Name Type.Variable


type alias Pools =
  MVector.MVector (TList Type.Variable)


type State =
  State
    {- env -} Env
    {- mark -} Type.Mark
    {- errors -} (TList Error.Error)


setEnv env (State _ a b) = State env a b


solve : Env -> Int -> Pools -> State -> Type.Constraint -> IO State
solve env rank pools state constraint =
  IO.loop solveHelp ((env, rank), (pools, state), (constraint, identity))


solveHelp : ((Env, Int), (Pools, State), (Type.Constraint, IO State -> IO State)) -> IO (IO.Step ((Env, Int), (Pools, State), (Type.Constraint, IO State -> IO State)) State)
solveHelp ((env, rank), (pools, state), (constraint, cont)) =
  case constraint of
    Type.CTrue ->
      IO.fmap IO.Done <| cont <| IO.return state

    Type.CSaveTheEnvironment ->
      IO.fmap IO.Done <| cont <| IO.return (setEnv env state)

    Type.CEqual region category tipe expectation ->
      IO.bind (typeToVariable rank pools tipe) <| \actual ->
      IO.bind (expectedToVariable rank pools expectation) <| \expected ->
      IO.bind (Unify.unify actual expected) <| \answer ->
      case answer of
        Unify.COk vars ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return state

        Unify.CErr vars actualType expectedType ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return <| addError state <|
            Error.BadExpr region category actualType <|
              Error.typeReplace expectation expectedType

    Type.CLocal region name expectation ->
      IO.bind (makeCopy rank pools (Map.ex env name)) <| \actual ->
      IO.bind (expectedToVariable rank pools expectation) <| \expected ->
      IO.bind (Unify.unify actual expected) <| \answer ->
      case answer of
        Unify.COk vars ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return state

        Unify.CErr vars actualType expectedType ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return <| addError state <|
            Error.BadExpr region (Error.Local name) actualType <|
              Error.typeReplace expectation expectedType

    Type.CForeign region name (Can.Forall freeVars srcType) expectation ->
      IO.bind (srcTypeToVariable rank pools freeVars srcType) <| \actual ->
      IO.bind (expectedToVariable rank pools expectation) <| \expected ->
      IO.bind (Unify.unify actual expected) <| \answer ->
      case answer of
        Unify.COk vars ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return state

        Unify.CErr vars actualType expectedType ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return <| addError state <|
            Error.BadExpr region (Error.Foreign name) actualType <|
              Error.typeReplace expectation expectedType

    Type.CPattern region category tipe expectation ->
      IO.bind (typeToVariable rank pools tipe) <| \actual ->
      IO.bind (patternExpectationToVariable rank pools expectation) <| \expected ->
      IO.bind (Unify.unify actual expected) <| \answer ->
      case answer of
        Unify.COk vars ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return state

        Unify.CErr vars actualType expectedType ->
          IO.bind (introduce rank pools vars) <| \_ ->
          IO.fmap IO.Done <| cont <| IO.return <| addError state <|
            Error.BadPattern region category actualType
              (Error.ptypeReplace expectation expectedType)

    Type.CAnd constraints ->
      IO.fmap IO.Done <| cont <| IO.foldlMList (solve env rank pools) state constraints

    Type.CLet [] flexs _ headerCon Type.CTrue ->
      IO.bind (introduce rank pools flexs) <| \_ ->
      IO.return <| IO.Loop ((env, rank), (pools, state), (headerCon, cont))

    Type.CLet [] [] header headerCon subCon ->
      IO.bind (solve env rank pools state headerCon) <| \state1 ->
      IO.bind (IO.traverseMap (A.traverse IO.fmap (typeToVariable rank pools)) header) <| \locals ->
      let newEnv = Map.union env (Map.map A.toValue locals) in
      IO.return <| IO.Loop ((newEnv, rank), (pools, state1), (subCon, IO.andThen (\state2 ->
      IO.foldlMList occurs state2 <| Map.toList locals) >> cont))

    Type.CLet rigids flexs header headerCon subCon ->
      -- work in the next pool to localize header
      let nextRank = rank + 1
          poolsLength = MVector.length pools in
      IO.bind
        (if nextRank < poolsLength
          then IO.return pools
          else liftP <| MVector.grow pools poolsLength) <| \nextPools ->

      -- introduce variables
      let vars = rigids ++ flexs in
      IO.bind (IO.forMList_ vars <| \var ->
        UF.modify var <| \(Type.Descriptor content _ mark copy) ->
          Type.Descriptor content nextRank mark copy) <| \_ ->
      IO.bind (liftP <| MVector.write nextPools nextRank vars) <| \_ ->

      -- run solver in next pool
      IO.bind (IO.traverseMap (A.traverse IO.fmap (typeToVariable nextRank nextPools)) header) <| \locals ->
      IO.bind (solve env nextRank nextPools state headerCon) <| \(State savedEnv mark errors) ->

      let youngMark = mark
          visitMark = Type.nextMark youngMark
          finalMark = Type.nextMark visitMark in

      -- pop pool
      IO.bind (generalize youngMark visitMark nextRank nextPools) <| \_ ->
      IO.bind (liftP <| MVector.write nextPools nextRank []) <| \_ ->

      -- check that things went well
      IO.bind (IO.mapMList_ isGeneric rigids) <| \_ ->

      let newEnv = Map.union env (Map.map A.toValue locals)
          tempState = State savedEnv finalMark errors in
      IO.return <| IO.Loop ((newEnv, rank), (nextPools, tempState), (subCon, IO.andThen (\newState ->

      IO.foldlMList occurs newState (Map.toList locals)) >> cont))


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric : Type.Variable -> IO ()
isGeneric var =
  IO.bind (UF.get var) <| \(Type.Descriptor _ rank _ _) ->
  if rank == Type.noRank
    then IO.return ()
    else
      IO.bind (Type.toErrorType var) <| \tipe ->
      Debug.todo <|
        "You ran into a compiler bug. Here are some details for the developers:\n\n"
        ++ "    " ++ D.toString (ET.toDoc L.empty RT.None tipe) ++ " [rank = " ++ String.fromInt rank ++ "]\n\n"
        ++
          "Please create an <http://sscce.org/> and then report it\n"
          ++ "at <https://github.com/elm/compiler/issues>\n\n"



-- EXPECTATIONS TO VARIABLE


expectedToVariable : Int -> Pools -> Error.Expected Type.Type -> IO Type.Variable
expectedToVariable rank pools expectation =
  typeToVariable rank pools <|
    case expectation of
      Error.NoExpectation tipe ->
        tipe

      Error.FromContext _ _ tipe ->
        tipe

      Error.FromAnnotation _ _ _ tipe ->
        tipe


patternExpectationToVariable : Int -> Pools -> Error.PExpected Type.Type -> IO Type.Variable
patternExpectationToVariable rank pools expectation =
  typeToVariable rank pools <|
    case expectation of
      Error.PNoExpectation tipe ->
        tipe

      Error.PFromContext _ _ tipe ->
        tipe



-- ERROR HELPERS


addError : State -> Error.Error -> State
addError (State savedEnv rank errors) err =
  State savedEnv rank (err::errors)



-- OCCURS CHECK


occurs : State -> (Name.Name, A.Located Type.Variable) -> IO State
occurs state (name, A.At region variable) =
  IO.bind (Occurs.occurs variable) <| \hasOccurred ->
  if hasOccurred
    then
      IO.bind (Type.toErrorType variable) <| \errorType ->
      IO.bind (UF.get variable) <| \(Type.Descriptor _ rank mark copy) ->
      IO.bind (UF.set variable (Type.Descriptor Type.Error rank mark copy)) <| \_ ->
      IO.return <| addError state (Error.InfiniteType region name errorType)
    else
      IO.return state



-- GENERALIZE


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize : Type.Mark -> Type.Mark -> Int -> Pools -> IO ()
generalize youngMark visitMark youngRank pools =
  IO.bind (liftP <| MVector.read pools youngRank) <| \youngVars ->
  IO.bind (poolToRankTable youngMark youngRank youngVars) <| \rankTable ->

  -- get the ranks right for each entry.
  -- start at low ranks so that we only have to pass
  -- over the information once.
  IO.bind (IO.imapMList_
    (\rank table -> IO.mapMList_ (adjustRank youngMark visitMark rank) table)
    rankTable) <| \_ ->

  -- For variables that have rank lowerer than youngRank, register them in
  -- the appropriate old pool if they are not redundant.
  IO.bind (IO.forMList_ (MList.init rankTable) <| \vars ->
    IO.forMList_ vars <| \var ->
      IO.bind (UF.redundant var) <| \isRedundant ->
      if isRedundant
        then IO.return ()
        else
          IO.bind (UF.get var) <| \(Type.Descriptor _ rank _ _) ->
          liftP <| MVector.modify pools (\l -> var::l) rank) <| \_ ->

  -- For variables with rank youngRank
  --   If rank < youngRank: register in oldPool
  --   otherwise generalize
  IO.forMList_ (MList.last rankTable) <| \var ->
    IO.bind (UF.redundant var) <| \isRedundant ->
    if isRedundant
      then IO.return ()
      else
        IO.bind (UF.get var) <| \(Type.Descriptor content rank mark copy) ->
        if rank < youngRank
          then liftP <| MVector.modify pools (\l -> var::l) rank
          else UF.set var <| Type.Descriptor content Type.noRank mark copy


poolToRankTable : Type.Mark -> Int -> TList Type.Variable -> IO (TList (TList Type.Variable))
poolToRankTable youngMark youngRank youngInhabitants =
  IO.bind (liftP <| MVector.replicate (youngRank + 1) []) <| \mutableTable ->

  -- Sort the youngPool variables into buckets by rank.
  IO.bind (IO.forMList_ youngInhabitants <| \var ->
    IO.bind (UF.get var) <| \(Type.Descriptor content rank _ copy) ->
    IO.bind (UF.set var (Type.Descriptor content rank youngMark copy)) <| \_ ->
    liftP <| MVector.modify mutableTable (\l -> var::l) rank) <| \_ ->

  liftP <| MVector.unsafeFreeze mutableTable



-- ADJUST RANK

--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--
adjustRank : Type.Mark -> Type.Mark -> Int -> Type.Variable -> Type.IO t Int
adjustRank youngMark visitMark groupRank var =
  IO.bind (UF.get var) <| \(Type.Descriptor content rank mark copy) ->
  if mark == youngMark then
    -- Set the variable as marked first because it may be cyclic.
    IO.bind (UF.set var <| Type.Descriptor content rank visitMark copy) <| \_ ->
    IO.bind (adjustRankContent youngMark visitMark groupRank content) <| \maxRank ->
    IO.bind (UF.set var <| Type.Descriptor content maxRank visitMark copy) <| \_ ->
    IO.return maxRank

  else if mark == visitMark then
    IO.return rank

  else
    let minRank = min groupRank rank in
    -- TODO how can minRank ever be groupRank?
    IO.bind (UF.set var <| Type.Descriptor content minRank visitMark copy) <| \_ ->
    IO.return minRank


adjustRankContent : Type.Mark -> Type.Mark -> Int -> Type.Content -> Type.IO t Int
adjustRankContent youngMark visitMark groupRank content =
  let
    go = adjustRank youngMark visitMark groupRank
  in
    case content of
      Type.FlexVar _ ->
        IO.return groupRank

      Type.FlexSuper _ _ ->
        IO.return groupRank

      Type.RigidVar _ ->
        IO.return groupRank

      Type.RigidSuper _ _ ->
        IO.return groupRank

      Type.Structure flatType ->
        case flatType of
          Type.App1 _ _ args ->
            IO.foldlMList (\rank arg -> IO.fmap (max rank) <| go arg) Type.outermostRank args

          Type.Fun1 arg result ->
            IO.liftA2 max (go arg) (go result)

          Type.EmptyRecord1 ->
            -- THEORY: an empty record never needs to get generalized
            IO.return Type.outermostRank

          Type.Record1 fields extension ->
            IO.bind (go extension) <| \extRank ->
            IO.foldlMMap (\rank field -> IO.fmap (max rank) <| go field) extRank fields

          Type.Unit1 ->
            -- THEORY: a unit never needs to get generalized
            IO.return Type.outermostRank

          Type.Tuple1 a b maybeC ->
            IO.bind (go a) <| \ma ->
            IO.bind (go b) <| \mb ->
            case maybeC of
              Nothing ->
                IO.return (max ma mb)

              Just c ->
                IO.fmap (max (max ma mb)) <| go c

      Type.Alias _ _ args _ ->
        -- THEORY: anything in the realVar would be outermostRank
        IO.foldlMList (\rank (_, argVar) -> IO.fmap (max rank) <| go argVar) Type.outermostRank args

      Type.Error ->
        IO.return groupRank



-- REGISTER VARIABLES


introduce : Int -> Pools -> (TList Type.Variable) -> IO ()
introduce rank pools variables =
  IO.bind (liftP <| MVector.modify pools (\l -> variables++l) rank) <| \_ ->
  IO.forMList_ variables <| \var ->
    UF.modify var <| \(Type.Descriptor content _ mark copy) ->
      Type.Descriptor content rank mark copy



-- TYPE TO VARIABLE


typeToVariable : Int -> Pools -> Type.Type -> IO Type.Variable
typeToVariable rank pools tipe =
  typeToVar rank pools tipe


-- PERF working with @mgriffith we noticed that a 784 line entry in a `let` was
-- causing a ~1.5 second slowdown. Moving it to the top-level to be a function
-- saved all that time. The slowdown seems to manifest in `typeToVar` and in
-- `register` in particular. Have not explored further yet. Top-level definitions
-- are recommended in cases like this anyway, so there is at least a safety
-- valve for now.
--
typeToVar : Int -> Pools -> Type.Type -> IO Type.Variable
typeToVar rank pools tipe =
  let go = typeToVar rank pools in
  case tipe of
    Type.VarN v ->
      IO.return v

    Type.AppN home name args ->
      IO.bind (IO.traverseList go args) <| \argVars ->
      register rank pools (Type.Structure (Type.App1 home name argVars))

    Type.FunN a b ->
      IO.bind (go a) <| \aVar ->
      IO.bind (go b) <| \bVar ->
      register rank pools (Type.Structure (Type.Fun1 aVar bVar))

    Type.AliasN home name args aliasType ->
      IO.bind (IO.traverseList (MTuple.traverseSecond IO.fmap go) args) <| \argVars ->
      IO.bind (typeToVar rank pools aliasType) <| \aliasVar ->
      register rank pools (Type.Alias home name argVars aliasVar)

    Type.RecordN fields ext ->
      IO.bind (IO.traverseMap go fields) <| \fieldVars ->
      IO.bind (go ext) <| \extVar ->
      register rank pools (Type.Structure (Type.Record1 fieldVars extVar))

    Type.EmptyRecordN ->
      register rank pools emptyRecord1

    Type.UnitN ->
      register rank pools unit1

    Type.TupleN a b c ->
      IO.bind (go a) <| \aVar ->
      IO.bind (go b) <| \bVar ->
      IO.bind (MMaybe.traverse IO.pure IO.fmap go c) <| \cVar ->
      register rank pools (Type.Structure (Type.Tuple1 aVar bVar cVar))


register : Int -> Pools -> Type.Content -> IO Type.Variable
register rank pools content =
  IO.bind (UF.fresh (Type.Descriptor content rank Type.noMark Nothing)) <| \var ->
  IO.bind (liftP <| MVector.modify pools (\l -> var::l) rank) <| \_ ->
  IO.return var


emptyRecord1 : Type.Content
emptyRecord1 =
  Type.Structure Type.EmptyRecord1


unit1 : Type.Content
unit1 =
  Type.Structure Type.Unit1



-- SOURCE TYPE TO VARIABLE


srcTypeToVariable : Int -> Pools -> Map.Map Name.Name () -> Can.Type -> IO Type.Variable
srcTypeToVariable rank pools freeVars srcType =
  let
    nameToContent name =
      if Name.isNumberType     name then Type.FlexSuper Type.Number (Just name)
      else if Name.isComparableType name then Type.FlexSuper Type.Comparable (Just name)
      else if Name.isAppendableType name then Type.FlexSuper Type.Appendable (Just name)
      else if Name.isCompappendType name then Type.FlexSuper Type.CompAppend (Just name)
      else Type.FlexVar (Just name)

    makeVar name _ =
      UF.fresh (Type.Descriptor (nameToContent name) rank Type.noMark Nothing)
  in
    IO.bind (IO.traverseWithKey makeVar freeVars) <| \flexVars ->
    IO.bind (liftP <| MVector.modify pools (\l -> Map.elems flexVars ++ l) rank) <| \_ ->
    srcTypeToVar rank pools flexVars srcType


srcTypeToVar : Int -> Pools -> Map.Map Name.Name Type.Variable -> Can.Type -> IO Type.Variable
srcTypeToVar rank pools flexVars srcType =
  let go = srcTypeToVar rank pools flexVars in
  case srcType of
    Can.TLambda argument result ->
      IO.bind (go argument) <| \argVar ->
      IO.bind (go result) <| \resultVar ->
      register rank pools (Type.Structure (Type.Fun1 argVar resultVar))

    Can.TVar name ->
      IO.return (Map.ex flexVars name)

    Can.TType home name args ->
      IO.bind (IO.traverseList go args) <| \argVars ->
      register rank pools (Type.Structure (Type.App1 home name argVars))

    Can.TRecord fields maybeExt ->
      IO.bind (IO.traverseMap (srcFieldTypeToVar rank pools flexVars) fields) <| \fieldVars ->
      IO.bind
        (case maybeExt of
          Nothing -> register rank pools emptyRecord1
          Just ext -> IO.return (Map.ex flexVars ext)
        ) <| \extVar ->
      register rank pools (Type.Structure (Type.Record1 fieldVars extVar))

    Can.TUnit ->
      register rank pools unit1

    Can.TTuple a b c ->
      IO.bind (go a) <| \aVar ->
      IO.bind (go b) <| \bVar ->
      IO.bind (MMaybe.traverse IO.pure IO.fmap go c) <| \cVar ->
      register rank pools (Type.Structure (Type.Tuple1 aVar bVar cVar))

    Can.TAlias home name args aliasType ->
      IO.bind (IO.traverseList (MTuple.traverseSecond IO.fmap go) args) <| \argVars ->
      IO.bind
        (case aliasType of
          Can.Holey tipe ->
            srcTypeToVar rank pools (Map.fromList argVars) tipe

          Can.Filled tipe ->
            go tipe
        ) <| \aliasVar ->

      register rank pools (Type.Alias home name argVars aliasVar)


srcFieldTypeToVar : Int -> Pools -> Map.Map Name.Name Type.Variable -> Can.FieldType -> IO Type.Variable
srcFieldTypeToVar rank pools flexVars (Can.FieldType _ srcTipe) =
  srcTypeToVar rank pools flexVars srcTipe



-- COPY


makeCopy : Int -> Pools -> Type.Variable -> IO Type.Variable
makeCopy rank pools var =
  IO.bind (makeCopyHelp rank pools var) <| \copy ->
  IO.bind (restore var) <| \_ ->
  IO.return copy


makeCopyHelp : Int -> Pools -> Type.Variable -> IO Type.Variable
makeCopyHelp maxRank pools variable =
  IO.bind (UF.get variable) <| \(Type.Descriptor content rank _ maybeCopy) ->

  case maybeCopy of
    Just copy ->
      IO.return copy

    Nothing ->
      if rank /= Type.noRank then
        IO.return variable

      else
        let makeDescriptor c = Type.Descriptor c maxRank Type.noMark Nothing in
        IO.bind (UF.fresh <| makeDescriptor content) <| \copy ->
        IO.bind (liftP <| MVector.modify pools (\l -> copy::l) maxRank) <| \_ ->

        -- Link the original variable to the new variable. This lets us
        -- avoid making multiple copies of the variable we are instantiating.
        --
        -- Need to do this before recursively copying to avoid looping.
        IO.bind (UF.set variable <|
          Type.Descriptor content rank Type.noMark (Just copy)) <| \_ ->

        -- Now we recursively copy the content of the variable.
        -- We have already marked the variable as copied, so we
        -- will not repeat this work or crawl this variable again.
        case content of
          Type.Structure term ->
            IO.bind (traverseFlatType (makeCopyHelp maxRank pools) term) <| \newTerm ->
            IO.bind (UF.set copy <| makeDescriptor (Type.Structure newTerm)) <| \_ ->
            IO.return copy

          Type.FlexVar _ ->
            IO.return copy

          Type.FlexSuper _ _ ->
            IO.return copy

          Type.RigidVar name ->
            IO.bind (UF.set copy <| makeDescriptor <| Type.FlexVar (Just name)) <| \_ ->
            IO.return copy

          Type.RigidSuper super name ->
            IO.bind (UF.set copy <| makeDescriptor <| Type.FlexSuper super (Just name)) <| \_ ->
            IO.return copy

          Type.Alias home name args realType ->
            IO.bind (IO.traverseList (MTuple.traverseSecond IO.fmap (makeCopyHelp maxRank pools)) args) <| \newArgs ->
            IO.bind (makeCopyHelp maxRank pools realType) <| \newRealType ->
            IO.bind (UF.set copy <| makeDescriptor (Type.Alias home name newArgs newRealType)) <| \_ ->
            IO.return copy

          Type.Error ->
            IO.return copy



-- RESTORE


restore : Type.Variable -> IO ()
restore variable =
  IO.bind (UF.get variable) <| \(Type.Descriptor content _ _ maybeCopy) ->
  case maybeCopy of
    Nothing ->
      IO.return ()

    Just _ ->
      IO.bind (UF.set variable <| Type.Descriptor content Type.noRank Type.noMark Nothing) <| \_ ->
      restoreContent content


restoreContent : Type.Content -> IO ()
restoreContent content =
  case content of
    Type.FlexVar _ ->
      IO.return ()

    Type.FlexSuper _ _ ->
      IO.return ()

    Type.RigidVar _ ->
      IO.return ()

    Type.RigidSuper _ _ ->
      IO.return ()

    Type.Structure term ->
      case term of
        Type.App1 _ _ args ->
          IO.mapMList_ restore args

        Type.Fun1 arg result ->
          IO.bind (restore arg) <| \_ ->
          restore result

        Type.EmptyRecord1 ->
          IO.return ()

        Type.Record1 fields ext ->
          IO.bind (IO.mapMMap_ restore fields) <| \_ ->
          restore ext

        Type.Unit1 ->
          IO.return ()

        Type.Tuple1 a b maybeC ->
          IO.bind (restore a) <| \_ ->
          IO.bind (restore b) <| \_ ->
          case maybeC of
            Nothing -> IO.return ()
            Just c  -> restore c

    Type.Alias _ _ args var ->
      IO.bind (IO.mapMList_ (MTuple.traverseSecond IO.fmap restore) args) <| \_ ->
      restore var

    Type.Error ->
      IO.return ()



-- TRAVERSE FLAT TYPE


traverseFlatType : (Type.Variable -> IO Type.Variable) -> Type.FlatType -> IO Type.FlatType
traverseFlatType f flatType =
  case flatType of
    Type.App1 home name args ->
      IO.liftM (Type.App1 home name) (IO.traverseList f args)

    Type.Fun1 a b ->
      IO.liftM2 Type.Fun1 (f a) (f b)

    Type.EmptyRecord1 ->
      IO.pure Type.EmptyRecord1

    Type.Record1 fields ext ->
      IO.liftM2 Type.Record1 (IO.traverseMap f fields) (f ext)

    Type.Unit1 ->
      IO.pure Type.Unit1

    Type.Tuple1 a b cs ->
      IO.liftM3 Type.Tuple1 (f a) (f b) (MMaybe.traverse IO.pure IO.fmap f cs)
