{- MANUALLY FORMATTED -}
module Builder.Deps.Solver exposing
  ( {-Solver
  ,-} Result(..)
  , Connection(..)
  ----
  , Details(..)
  , verify
  ----
  , AppSolution(..)
  , addToApp
  --
  , Env(..)
  , initEnv
  )


import Builder.Deps.Registry as Registry
import Builder.Deps.Website as Website
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Extra.System.Dir as Dir
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Class.Monad as Monad



-- PRIVATE IO


type alias IO c d e f g h v =
  IO.IO (Dir.GlobalState c d e f g h) v



-- SOLVER


type Solver z b c d e f g h v =
  Solver
  (
    State
    -> (State -> v -> (State -> IO c d e f g h z) -> IO c d e f g h z)
    -> (State -> IO c d e f g h z)
    -> (Exit.Solver -> IO c d e f g h z)
    -> IO c d e f g h z
  )


type State =
  State
    {- cache -} Stuff.PackageCache
    {- connection -} Connection
    {- registry -} Registry.Registry
    {- constraints -} (Map.Map (Pkg.Comparable, V.Comparable) Constraints)


type Constraints =
  Constraints
    {- elm -} (C.Constraint)
    {- deps -} (Map.Map Pkg.Comparable C.Constraint)

getDeps (Constraints _ deps) = deps


type Connection
  = Online Http.Manager
  | Offline



-- RESULT


type Result a
  = Ok a
  | NoSolution
  | NoOfflineSolution
  | Err Exit.Solver



-- VERIFY -- used by Elm.Details


type Details =
  Details V.Version (Map.Map Pkg.Comparable C.Constraint)


verify : Stuff.PackageCache -> Connection -> Registry.Registry -> Map.Map Pkg.Comparable C.Constraint -> IO c d e f g h (Result (Map.Map Pkg.Comparable Details))
verify cache connection registry constraints =
  case try constraints of
    Solver solver ->
      solver (State cache connection registry Map.empty)
        (\s a _ -> IO.return <| Ok (Map.mapWithKey (addDeps s) a))
        (\_     -> IO.return <| noSolution connection)
        (\e     -> IO.return <| Err e)


addDeps : State -> Pkg.Comparable -> V.Version -> Details
addDeps (State _ _ _ constraints) name vsn =
  case Map.lookup (name, V.toComparable vsn) constraints of
    Just (Constraints _ deps) -> Details vsn deps
    Nothing                   -> Debug.todo "compiler bug manifesting in Deps.Solver.addDeps"


noSolution : Connection -> Result a
noSolution connection =
  case connection of
    Online _ -> NoSolution
    Offline -> NoOfflineSolution



-- ADD TO APP - used in Install


type AppSolution =
  AppSolution
    {- old -} (Map.Map Pkg.Comparable V.Version)
    {- new -} (Map.Map Pkg.Comparable V.Version)
    {- app -} (Outline.AppOutline)


addToApp : Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Comparable -> Outline.AppOutline -> IO c d e f g h (Result AppSolution)
addToApp cache connection registry pkg ((Outline.AppOutline _ _ direct indirect testDirect testIndirect) as outline) =
  let
    allIndirects = Map.union indirect testIndirect
    allDirects = Map.union direct testDirect
    allDeps = Map.union allDirects allIndirects

    attempt toConstraint deps =
      try (Map.insert pkg C.anything (Map.map toConstraint deps))
  in
  case
    oneOf
      ( attempt C.exactly allDeps )
      [ attempt C.exactly allDirects
      , attempt C.untilNextMinor allDirects
      , attempt C.untilNextMajor allDirects
      , attempt (\_ -> C.anything) allDirects
      ]
  of
    Solver solver ->
      solver (State cache connection registry Map.empty)
        (\s a _ -> IO.return <| Ok (toApp s pkg outline allDeps a))
        (\_     -> IO.return <| noSolution connection)
        (\e     -> IO.return <| Err e)


toApp : State -> Pkg.Comparable -> Outline.AppOutline -> Map.Map Pkg.Comparable V.Version -> Map.Map Pkg.Comparable V.Version -> AppSolution
toApp (State _ _ _ constraints) pkg (Outline.AppOutline elm srcDirs direct _ testDirect _) old new =
  let
    d   = Map.intersection new (Map.insert pkg V.one direct)
    i   = Map.difference (getTransitive constraints new (Map.toList d) Map.empty) d
    td  = Map.intersection new (Map.delete pkg testDirect)
    ti  = Map.difference new (Map.unions MList.foldl [d,i,td])
  in
  AppSolution old new (Outline.AppOutline elm srcDirs d i td ti)


getTransitive : Map.Map (Pkg.Comparable, V.Comparable) Constraints -> Map.Map Pkg.Comparable V.Version -> TList (Pkg.Comparable,V.Version) -> Map.Map Pkg.Comparable V.Version -> Map.Map Pkg.Comparable V.Version
getTransitive constraints solution unvisited visited =
  case unvisited of
    [] ->
      visited

    (pkg,vsn) :: infos ->
      if Map.member pkg visited
      then getTransitive constraints solution infos visited
      else
        let
          newDeps = getDeps (Map.ex constraints (pkg, V.toComparable vsn))
          newUnvisited = Map.toList (Map.intersection solution (Map.difference newDeps visited))
          newVisited = Map.insert pkg vsn visited
        in
        getTransitive constraints solution infos <|
          getTransitive constraints solution newUnvisited newVisited



-- TRY


try : Map.Map Pkg.Comparable C.Constraint -> Solver z b c d e f g h (Map.Map Pkg.Comparable V.Version)
try constraints =
  exploreGoals (Goals constraints Map.empty)



-- EXPLORE GOALS


type Goals =
  Goals
    {- pending -} (Map.Map Pkg.Comparable C.Constraint)
    {- solved -} (Map.Map Pkg.Comparable V.Version)


exploreGoals : Goals -> Solver z b c d e f g h (Map.Map Pkg.Comparable V.Version)
exploreGoals (Goals pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved

    Just ((name, constraint), otherPending) ->
      let goals1 = Goals otherPending solved in
      let addVsn = addVersion goals1 name in
      bind (getRelevantVersions name constraint) <| \(v,vs) ->
      bind (oneOf (addVsn v) (MList.map addVsn vs)) <| \goals2 ->
      exploreGoals goals2


addVersion : Goals -> Pkg.Comparable -> V.Version -> Solver z b c d e f g h Goals
addVersion (Goals pending solved) name version =
  bind (getConstraints (Pkg.fromComparable name) version) <| \(Constraints elm deps) ->
  if C.goodElm elm
    then
      bind (MList.foldlM return bind (addConstraint solved) pending (Map.toList deps)) <| \newPending ->
      return (Goals newPending (Map.insert name version solved))
    else
      backtrack


addConstraint : Map.Map Pkg.Comparable V.Version -> Map.Map Pkg.Comparable C.Constraint -> (Pkg.Comparable, C.Constraint) -> Solver z b c d e f g h (Map.Map Pkg.Comparable C.Constraint)
addConstraint solved unsolved (name, newConstraint) =
  case Map.lookup name solved of
    Just version ->
      if C.satisfies newConstraint version
      then return unsolved
      else backtrack

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return <| Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              backtrack

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
              then return unsolved
              else return (Map.insert name mergedConstraint unsolved)



-- GET RELEVANT VERSIONS


getRelevantVersions : Pkg.Comparable -> C.Constraint -> Solver z b c d e f g h (V.Version, TList V.Version)
getRelevantVersions name constraint =
  Solver <| \((State _ _ registry _) as state) ok back _ ->
    case Registry.getVersions name registry of
      Just (Registry.KnownVersions newest previous) ->
        case MList.filter (C.satisfies constraint) (newest::previous) of
          []   -> back state
          v::vs -> ok state (v,vs) back

      Nothing ->
        back state



-- GET CONSTRAINTS


getConstraints : Pkg.Name -> V.Version -> Solver z b c d e f g h Constraints
getConstraints pkg vsn =
  Solver <| \((State cache connection registry cDict) as state) ok back err ->
    let key = (Pkg.toComparable pkg, V.toComparable vsn) in
    case Map.lookup key cDict of
      Just cs ->
        ok state cs back

      Nothing ->
        let toNewState cs = State cache connection registry (Map.insert key cs cDict) in
        let home = Stuff.package cache pkg vsn in
        let path = Dir.addName home "elm.json"in
        IO.bind (File.exists path) <| \outlineExists ->
        if outlineExists
          then
            IO.bind (File.readUtf8 path) <| \bytes ->
            case D.fromByteString constraintsDecoder bytes of
              Right cs ->
                case connection of
                  Online _ ->
                    ok (toNewState cs) cs back

                  Offline ->
                    IO.bind (Dir.doesDirectoryExist (Dir.addName (Stuff.package cache pkg vsn) "src")) <| \srcExists ->
                    if srcExists
                      then ok (toNewState cs) cs back
                      else back state

              Left  _  ->
                IO.bind (File.remove path) <| \_ ->
                err (Exit.SolverBadCacheData pkg vsn)
          else
            case connection of
              Offline ->
                back state

              Online manager ->
                let url = Website.metadata pkg vsn "elm.json" in
                IO.bind (Http.get manager url [] identity (IO.return << Right)) <| \result ->
                case result of
                  Left httpProblem ->
                    err (Exit.SolverBadHttp pkg vsn httpProblem)

                  Right body ->
                    case D.fromByteString constraintsDecoder body of
                      Right cs ->
                        IO.bind (Dir.createDirectoryIfMissing True home) <| \_ ->
                        IO.bind (File.writeUtf8 path body) <| \_ ->
                        ok (toNewState cs) cs back

                      Left _ ->
                        err (Exit.SolverBadHttpData pkg vsn url)


constraintsDecoder : D.Decoder () Constraints
constraintsDecoder =
  D.bind (D.mapError (always ()) Outline.decoder) <| \outline ->
  case outline of
    Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps _ elmConstraint) ->
      D.return (Constraints elmConstraint deps)

    Outline.App _ ->
      D.failure ()



-- ENVIRONMENT


type Env =
  Env Stuff.PackageCache Http.Manager Connection Registry.Registry


initEnv : IO c d e f g h (Either Exit.RegistryProblem Env)
initEnv =
  IO.bind Http.getManager <| \manager ->
  IO.bind Stuff.getPackageCache <| \cache ->
  IO.bind (Registry.read cache) <| \maybeRegistry ->

  case maybeRegistry of
    Nothing ->
      IO.bind (Registry.fetch manager cache) <| \eitherRegistry ->
      case eitherRegistry of
        Right latestRegistry ->
          IO.return <| Right <| Env cache manager (Online manager) (addSpecialVersions latestRegistry)

        Left problem ->
          IO.return <| Left <| problem

    Just cachedRegistry ->
      IO.bind (Registry.update manager cache cachedRegistry) <| \eitherRegistry ->
      case eitherRegistry of
        Right latestRegistry ->
          IO.return <| Right <| Env cache manager (Online manager) (addSpecialVersions latestRegistry)

        Left _ ->
          IO.return <| Right <| Env cache manager Offline (addSpecialVersions cachedRegistry)


{- NEW: addSpecialVersions -}
addSpecialVersions : Registry.Registry -> Registry.Registry
addSpecialVersions (Registry.Registry count registry) =
  Registry.Registry
    (count + 1)
    (Map.insert ("elm", "breakpoint") (Registry.KnownVersions V.one []) registry)



-- INSTANCES


return : Monad.Return v (Solver z b c d e f g h v)
return a =
  Solver <| \state ok back _ -> ok state a back

bind : Monad.Bind v (Solver z b c d e f g h v) (Solver z b c d e f g h w)
bind (Solver solverA) callback =
  Solver <| \state ok back err ->
    let
      okA stateA a backA =
        case callback a of
          Solver solverB -> solverB stateA ok backA err
    in
    solverA state okA back err


oneOf : Solver z b c d e f g h v -> TList (Solver z b c d e f g h v) -> Solver z b c d e f g h v
oneOf ((Solver solverHead) as solver) solvers =
  case solvers of
    [] ->
      solver

    s::ss ->
      Solver <| \state0 ok back err ->
        let
          tryTail state1 =
            let
              (Solver solverTail) = oneOf s ss
            in
            solverTail state1 ok back err
        in
        solverHead state0 ok tryTail err


backtrack : Solver z b c d e f g h v
backtrack =
  Solver <| \state _ back _ -> back state
