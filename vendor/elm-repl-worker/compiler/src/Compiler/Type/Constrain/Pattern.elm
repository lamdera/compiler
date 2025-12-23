{- MANUALLY FORMATTED -}
module Compiler.Type.Constrain.Pattern exposing
  ( State(..)
  , emptyState
  , add
  )


import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as T
import Extra.System.IO.Pure as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- IO


type alias IO t a = T.IO t a



-- ACTUALLY ADD CONSTRAINTS


-- The constraints are stored in reverse order so that adding a new
-- constraint is O(1) and we can reverse it at some later time.
--
type State =
  State
    {- headers -} Header
    {- vars -} (TList T.Variable)
    {- revCons -} (TList T.Constraint)


type alias Header = Map.Map Name.Name (A.Located T.Type)


add : Can.Pattern -> E.PExpected T.Type -> State -> IO t State
add (A.At region pattern) expectation state =
  case pattern of
    Can.PAnything ->
      IO.return state

    Can.PVar name ->
      IO.return <| addToHeaders region name expectation state

    Can.PAlias realPattern name ->
      add realPattern expectation <|
        addToHeaders region name expectation state

    Can.PUnit ->
      let (State headers vars revCons) = state
          unitCon = T.CPattern region E.PUnit T.UnitN expectation in
      IO.return <| State headers vars (unitCon::revCons)

    Can.PTuple a b maybeC ->
      addTuple region a b maybeC expectation state

    Can.PCtor home typeName (Can.Union typeVars _ _ _) ctorName _ args ->
      addCtor region home typeName typeVars ctorName args expectation state

    Can.PList patterns ->
      IO.bind T.mkFlexVar <| \entryVar ->
      let entryType = T.VarN entryVar
          listType = T.AppN ModuleName.list Name.list [entryType] in

      IO.bind
        (IO.foldlMList (addEntry region entryType) state (Index.indexedMap Tuple.pair patterns)) <| \(State headers vars revCons) ->

      let listCon = T.CPattern region E.PList listType expectation in
      IO.return <| State headers (entryVar::vars) (listCon::revCons)

    Can.PCons headPattern tailPattern ->
      IO.bind T.mkFlexVar <| \entryVar ->
      let entryType = T.VarN entryVar
          listType = T.AppN ModuleName.list Name.list [entryType]

          headExpectation = E.PNoExpectation entryType
          tailExpectation = E.PFromContext region E.PTail listType in

      IO.bind
        (IO.andThen (add headPattern headExpectation) <|
          add tailPattern tailExpectation state) <| \(State headers vars revCons) ->

      let listCon = T.CPattern region E.PList listType expectation in
      IO.return <| State headers (entryVar::vars) (listCon :: revCons)

    Can.PRecord fields ->
      IO.bind T.mkFlexVar <| \extVar ->
      let extType = T.VarN extVar in

      IO.bind (IO.traverseList (\field -> IO.fmap (Tuple.pair field) <| T.mkFlexVar) fields) <| \fieldVars ->
      let fieldTypes = Map.fromList (MList.map (Tuple.mapSecond T.VarN) fieldVars)
          recordType = T.RecordN fieldTypes extType

          (State headers vars revCons) = state
          recordCon = T.CPattern region E.PRecord recordType expectation in
      IO.return <|
        State
          {- headers -} (Map.union headers (Map.map (A.At region) fieldTypes))
          {- vars -} (MList.map Tuple.second fieldVars ++ extVar :: vars)
          {- revCons -} (recordCon :: revCons)

    Can.PInt _ ->
      let (State headers vars revCons) = state
          intCon = T.CPattern region E.PInt T.int expectation in
      IO.return <| State headers vars (intCon::revCons)

    Can.PStr _ ->
      let (State headers vars revCons) = state
          strCon = T.CPattern region E.PStr T.string expectation in
      IO.return <| State headers vars (strCon::revCons)

    Can.PChr _ ->
      let (State headers vars revCons) = state
          chrCon = T.CPattern region E.PChr T.char expectation in
      IO.return <| State headers vars (chrCon::revCons)

    Can.PBool _ _ ->
      let (State headers vars revCons) = state
          boolCon = T.CPattern region E.PBool T.bool expectation in
      IO.return <| State headers vars (boolCon::revCons)



-- STATE HELPERS


emptyState : State
emptyState =
  State Map.empty [] []


addToHeaders : A.Region -> Name.Name -> E.PExpected T.Type -> State -> State
addToHeaders region name expectation (State headers vars revCons) =
  let
    tipe = getType expectation
    newHeaders = Map.insert name (A.At region tipe) headers
  in
  State newHeaders vars revCons


getType : E.PExpected T.Type -> T.Type
getType expectation =
  case expectation of
    E.PNoExpectation tipe -> tipe
    E.PFromContext _ _ tipe -> tipe



-- CONSTRAIN LIST


addEntry : A.Region -> T.Type -> State -> (Index.ZeroBased, Can.Pattern) -> IO t State
addEntry listRegion tipe state (index, pattern) =
  let
    expectation =
      E.PFromContext listRegion (E.PListEntry index) tipe
  in
  add pattern expectation state



-- CONSTRAIN TUPLE


addTuple : A.Region -> Can.Pattern -> Can.Pattern -> Maybe Can.Pattern -> E.PExpected T.Type -> State -> IO t State
addTuple region a b maybeC expectation state =
  IO.bind T.mkFlexVar <| \aVar ->
  IO.bind T.mkFlexVar <| \bVar ->
  let aType = T.VarN aVar
      bType = T.VarN bVar in

  case maybeC of
    Nothing ->
      IO.bind
        (IO.andThen (simpleAdd b bType) <|
          simpleAdd a aType state) <| \(State headers vars revCons) ->

      let tupleCon = T.CPattern region E.PTuple (T.TupleN aType bType Nothing) expectation in

      IO.return <| State headers (aVar::bVar::vars) (tupleCon::revCons)

    Just c ->
      IO.bind T.mkFlexVar <| \cVar ->
      let cType = T.VarN cVar in

      IO.bind
        (IO.andThen (simpleAdd c cType) <|
          IO.andThen (simpleAdd b bType) <|
            simpleAdd a aType state) <| \(State headers vars revCons) ->

      let tupleCon = T.CPattern region E.PTuple (T.TupleN aType bType (Just cType)) expectation in

      IO.return <| State headers (aVar::bVar::cVar::vars) (tupleCon::revCons)


simpleAdd : Can.Pattern -> T.Type -> State -> IO t State
simpleAdd pattern patternType state =
  add pattern (E.PNoExpectation patternType) state



-- CONSTRAIN CONSTRUCTORS


addCtor : A.Region -> ModuleName.Canonical -> Name.Name -> TList Name.Name -> Name.Name -> TList Can.PatternCtorArg -> E.PExpected T.Type -> State -> IO t State
addCtor region home typeName typeVarNames ctorName args expectation state =
  IO.bind (IO.traverseList (\var -> IO.fmap (Tuple.pair var) <| T.nameToFlex var) typeVarNames) <| \varPairs ->
  let typePairs = MList.map (Tuple.mapSecond T.VarN) varPairs
      freeVarDict = Map.fromList typePairs in

  IO.bind
    (IO.foldlMList (addCtorArg region ctorName freeVarDict) state args) <| \(State headers vars revCons) ->

  let ctorType = T.AppN home typeName (MList.map Tuple.second typePairs)
      ctorCon = T.CPattern region (E.PCtor ctorName) ctorType expectation in

  IO.return <|
    State
      {- headers -} headers
      {- vars -} (MList.map Tuple.second varPairs ++ vars)
      {- revCons -} (ctorCon :: revCons)


addCtorArg : A.Region -> Name.Name -> Map.Map Name.Name T.Type -> State -> Can.PatternCtorArg -> IO t State
addCtorArg region ctorName freeVarDict state (Can.PatternCtorArg index srcType pattern) =
  IO.bind (Instantiate.fromSrcType freeVarDict srcType) <| \tipe ->
  let expectation = E.PFromContext region (E.PCtorArg ctorName index) tipe in
  add pattern expectation state
