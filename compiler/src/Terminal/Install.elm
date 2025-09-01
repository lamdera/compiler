{- MANUALLY FORMATTED -}
module Terminal.Install exposing
  ( install
  )


import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D exposing (d)
import Extra.System.Dir exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Terminal.Command as Command



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Command.GlobalState g h) v



-- INSTALL

install : FilePath -> Pkg.Name -> IO g h (Either Exit.Install ())
install root pkg =
  Task.run <|
    Task.bind (Task.eio Exit.InstallBadRegistry Solver.initEnv) <| \env ->
    Task.bind (Task.eio Exit.InstallBadOutline <| Outline.read root) <| \oldOutline ->
    case oldOutline of
    Outline.App outline ->
      Task.bind (makeAppPlan env (Pkg.toComparable pkg) outline) <| \changes ->
      attemptChanges root env oldOutline V.toChars changes

    Outline.Pkg outline ->
      Task.bind (makePkgPlan env (Pkg.toComparable pkg) outline) <| \changes ->
      attemptChanges root env oldOutline C.toChars changes



-- ATTEMPT CHANGES


type Changes vsn
  = AlreadyInstalled
  | PromoteTest Outline.Outline
  | PromoteIndirect Outline.Outline
  | Changes (Map.Map Pkg.Comparable (Change vsn)) Outline.Outline


type alias Task z g h v =
  Task.Task z (Command.GlobalState g h) Exit.Install v


attemptChanges : FilePath -> Solver.Env -> Outline.Outline -> (v -> String) -> Changes v -> Task z g h ()
attemptChanges root env oldOutline toChars changes =
  case changes of
    AlreadyInstalled ->
      Task.io <| Command.putLine "It is already installed!"

    PromoteIndirect newOutline ->
      attemptChangesHelp root env oldOutline newOutline <|
        D.vcat
          [ D.fillSep
            [d"I",d"found",d"it",d"in",d"your",d"elm.json",d"file,"
            ,d"but",d"in",d"the",D.dullyellowS "\"indirect\"",d"dependencies."
            ]
          , D.fillSep
            [d"Should",d"I",d"move",d"it",d"into",D.greenS "\"direct\""
            ,d"dependencies",d"for",d"more",d"general",d"use?",d"[Y/n]: "
            ]
          ]

    PromoteTest newOutline ->
      attemptChangesHelp root env oldOutline newOutline <|
        D.vcat
          [ D.fillSep
            [d"I",d"found",d"it",d"in",d"your",d"elm.json",d"file,"
            ,d"but",d"in",d"the",D.dullyellowS "\"test-dependencies\"",d"field."
            ]
          , D.fillSep
            [d"Should",d"I",d"move",d"it",d"into",D.greenS "\"dependencies\""
            ,d"for",d"more",d"general",d"use?",d"[Y/n]: "
            ]
          ]

    Changes changeDict newOutline ->
      let
        widths = Map.foldrWithKey (widen toChars) (Widths 0 0 0) changeDict
        changeDocs = Map.foldrWithKey (addChange toChars widths) (Docs [] [] []) changeDict
      in
      attemptChangesHelp root env oldOutline newOutline <| D.vcat <|
        [ d"Here is my plan:"
        , viewChangeDocs changeDocs
        , d""
        , d"Would you like me to update your elm.json accordingly? [Y/n]: "
        ]


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> D.Doc -> Task z g h ()
attemptChangesHelp root env oldOutline newOutline question =
  Task.eio Exit.InstallBadDetails <|
    IO.bind (Command.ask question) <| \approved ->
    if approved
    then
      IO.bind (Outline.write root newOutline) <| \_ ->
      IO.bind (Details.verifyInstall root env newOutline) <| \result ->
      case result of
      Left exit ->
        IO.bind (Outline.write root oldOutline) <| \_ ->
        IO.return (Left exit)

      Right () ->
        IO.bind (Command.putLine "Success!") <| \_ ->
        IO.return (Right ())
    else
      IO.bind (Command.putLine "Okay, I did not change anything!") <| \_ ->
      IO.return (Right ())



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Comparable -> Outline.AppOutline -> Task z g h (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline a b direct indirect testDirect testIndirect) as outline) =
  if Map.member pkg direct then
    Task.return AlreadyInstalled

  else
    -- is it already indirect?
    case Map.lookup pkg indirect of
      Just vsn ->
        Task.return <| PromoteIndirect <| Outline.App <|
          Outline.AppOutline a b
            {- app_deps_direct -} (Map.insert pkg vsn direct)
            {- app_deps_indirect -} (Map.delete pkg indirect)
            {- app_test_direct -} testDirect
            {- app_test_indirect -} testIndirect

      Nothing ->
        -- is it already a test dependency?
        case Map.lookup pkg testDirect of
          Just vsn ->
            Task.return <| PromoteTest <| Outline.App <|
              Outline.AppOutline a b
                {- app_deps_direct -} (Map.insert pkg vsn direct)
                {- app_deps_indirect -} indirect
                {- app_test_direct -} (Map.delete pkg testDirect)
                {- app_test_indirect -} testIndirect

          Nothing ->
            -- is it already an indirect test dependency?
            case Map.lookup pkg testIndirect of
              Just vsn ->
                Task.return <| PromoteTest <| Outline.App <|
                  Outline.AppOutline a b
                    {- app_deps_direct -} (Map.insert pkg vsn direct)
                    {- app_deps_indirect -} indirect
                    {- app_test_direct -} testDirect
                    {- app_test_indirect -} (Map.delete pkg testIndirect)

              Nothing ->
                -- finally try to add it from scratch
                case Registry.getVersionsE pkg registry of
                  Left suggestions ->
                    case connection of
                      Solver.Online _ -> Task.throw (Exit.InstallUnknownPackageOnline (Pkg.fromComparable pkg) suggestions)
                      Solver.Offline  -> Task.throw (Exit.InstallUnknownPackageOffline (Pkg.fromComparable pkg) suggestions)

                  Right _ ->
                    Task.bind (Task.io <| Solver.addToApp cache connection registry pkg outline) <| \result ->
                    case result of
                      Solver.Ok (Solver.AppSolution old new app) ->
                        Task.return (Changes (detectChanges old new) (Outline.App app))

                      Solver.NoSolution ->
                        Task.throw (Exit.InstallNoOnlineAppSolution (Pkg.fromComparable pkg))

                      Solver.NoOfflineSolution ->
                        Task.throw (Exit.InstallNoOfflineAppSolution (Pkg.fromComparable pkg))

                      Solver.Err exit ->
                        Task.throw (Exit.InstallHadSolverTrouble exit)



-- MAKE PACKAGE PLAN


makePkgPlan : Solver.Env -> Pkg.Comparable -> Outline.PkgOutline -> Task z g h (Changes C.Constraint)
makePkgPlan (Solver.Env cache _ connection registry) pkg (Outline.PkgOutline a b c d e deps test h) =
  if Map.member pkg deps then
    Task.return AlreadyInstalled
  else
    -- is already in test dependencies?
    case Map.lookup pkg test of
      Just con ->
        Task.return <| PromoteTest <| Outline.Pkg <|
          Outline.PkgOutline a b c d e
            {- pkg_deps -} (Map.insert pkg con deps)
            {- pkg_test_deps -} (Map.delete pkg test)
            h

      Nothing ->
        -- try to add a new dependency
        case Registry.getVersionsE pkg registry of
          Left suggestions ->
            case connection of
              Solver.Online _ -> Task.throw (Exit.InstallUnknownPackageOnline (Pkg.fromComparable pkg) suggestions)
              Solver.Offline  -> Task.throw (Exit.InstallUnknownPackageOffline (Pkg.fromComparable pkg) suggestions)

          Right (Registry.KnownVersions _ _) ->
            let old = Map.union deps test in
            let cons = Map.insert pkg C.anything old in
            Task.bind (Task.io <| Solver.verify cache connection registry cons) <| \result ->
            case result of
              Solver.Ok solution ->
                let
                  (Solver.Details vsn _) = Map.ex solution pkg

                  con = C.untilNextMajor vsn
                  new = Map.insert pkg con old
                  changes = detectChanges old new
                  news = Map.mapMaybe keepNew changes
                in
                Task.return <| Changes changes <| Outline.Pkg <|
                  Outline.PkgOutline a b c d e
                    {- pkg_deps -} (addNews (Just pkg) news deps)
                    {- pkg_test_deps -} (addNews Nothing news test)
                    h

              Solver.NoSolution ->
                Task.throw (Exit.InstallNoOnlinePkgSolution (Pkg.fromComparable pkg))

              Solver.NoOfflineSolution ->
                Task.throw (Exit.InstallNoOfflinePkgSolution (Pkg.fromComparable pkg))

              Solver.Err exit ->
                Task.throw (Exit.InstallHadSolverTrouble exit)


addNews : Maybe Pkg.Comparable -> Map.Map Pkg.Comparable C.Constraint -> Map.Map Pkg.Comparable C.Constraint -> Map.Map Pkg.Comparable C.Constraint
addNews pkg new old =
  Map.mergeA identity identity
    (Map.preserveMissing identity)
    (Map.mapMaybeMissing identity (\k c -> if Just k == pkg then Just c else Nothing))
    (Map.zipWithMatched identity (\_ _ n -> n))
    old
    new



-- CHANGES


type Change a
  = Insert a
  | Change a a
  | Remove a


detectChanges : Map.Map Pkg.Comparable a -> Map.Map Pkg.Comparable a -> Map.Map Pkg.Comparable (Change a)
detectChanges old new =
  Map.mergeA identity identity
    (Map.mapMissing identity (\_ v -> Remove v))
    (Map.mapMissing identity (\_ v -> Insert v))
    (Map.zipWithMaybeMatched identity keepChange)
    old
    new


keepChange : k -> v -> v -> Maybe (Change v)
keepChange _ old new =
  if old == new then
    Nothing
  else
    Just (Change old new)


keepNew : Change a -> Maybe a
keepNew change =
  case change of
    Insert a ->
      Just a

    Change _ a ->
      Just a

    Remove _ ->
      Nothing



-- VIEW CHANGE DOCS


type ChangeDocs =
  Docs
    {- doc_inserts -} (TList D.Doc)
    {- doc_changes -} (TList D.Doc)
    {- doc_removes -} (TList D.Doc)


viewChangeDocs : ChangeDocs -> D.Doc
viewChangeDocs (Docs inserts changes removes) =
  D.indent 2 <| D.vcat <| MList.concat <|
    [ viewNonZero "Add:"    inserts
    , viewNonZero "Change:" changes
    , viewNonZero "Remove:" removes
    ]


viewNonZero : String -> TList D.Doc -> TList D.Doc
viewNonZero title entries =
  if MList.null entries then
    []
  else
    [ d""
    , D.fromChars title
    , D.indent 2 (D.vcat entries)
    ]



-- VIEW CHANGE


addChange : (a -> String) -> Widths -> Pkg.Comparable -> Change a -> ChangeDocs -> ChangeDocs
addChange toChars widths name change (Docs inserts changes removes) =
  case change of
    Insert new ->
      Docs (viewInsert toChars widths name new :: inserts) changes removes

    Change old new ->
      Docs inserts (viewChange toChars widths name old new :: changes) removes

    Remove old ->
      Docs inserts changes (viewRemove toChars widths name old :: removes)


viewInsert : (a -> String) -> Widths -> Pkg.Comparable -> a -> D.Doc
viewInsert toChars (Widths nameWidth leftWidth _) name new =
  D.hsep [viewName nameWidth name, pad leftWidth (toChars new)]


viewChange : (a -> String) -> Widths -> Pkg.Comparable -> a -> a -> D.Doc
viewChange toChars (Widths nameWidth leftWidth rightWidth) name old new =
  D.hsep
    [ viewName nameWidth name
    , pad leftWidth (toChars old)
    , d"=>"
    , pad rightWidth (toChars new)
    ]


viewRemove : (a -> String) -> Widths -> Pkg.Comparable -> a -> D.Doc
viewRemove toChars (Widths nameWidth leftWidth _) name old =
  D.hsep [viewName nameWidth name, pad leftWidth (toChars old)]


viewName : Int -> Pkg.Comparable -> D.Doc
viewName width name =
  D.fromChars <| String.padRight (width + 3) ' ' (Pkg.toChars (Pkg.fromComparable name))


pad : Int -> String -> D.Doc
pad width string =
  D.da [D.fromChars (String.repeat (width - String.length string) " "), D.fromChars string]



-- WIDTHS


type Widths =
  Widths
    {- name -} Int
    {- left -} Int
    {- right -} Int


widen : (a -> String) -> Pkg.Comparable -> Change a -> Widths -> Widths
widen toChars pkg change (Widths name left right) =
  let
    toLength a =
      String.length (toChars a)

    newName =
      max name (String.length (Pkg.toChars (Pkg.fromComparable pkg)))
  in
    case change of
      Insert new ->
        Widths newName (max left (toLength new)) right

      Change old new ->
        Widths newName (max left (toLength old)) (max right (toLength new))

      Remove old ->
        Widths newName (max left (toLength old)) right
