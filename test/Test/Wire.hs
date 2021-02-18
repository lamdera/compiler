{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Wire where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Utf8 as Utf8

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))

import EasyTest
import Lamdera
import Lamdera.Evergreen.Snapshot
import NeatInterpolation
-- import qualified TestLamdera
import qualified Lamdera.Compile

import StandaloneInstances

-- Test all
import qualified Reporting.Exit as Exit
import qualified System.Exit as Exit
import qualified Reporting.Task as Task
import qualified Deps.Solver as Solver
import qualified Deps.Registry

import qualified BackgroundWriter as BW
import qualified Elm.Outline as Outline
import qualified Elm.Details as Details
import qualified Install
import qualified Reporting.Task as Task
import qualified Reporting.Doc as D
import qualified Elm.Version as V
import qualified Elm.Constraint as C
import qualified Stuff
import qualified Reporting

import qualified Init
import qualified Make

-- http
import qualified Json.Decode as D
import qualified Lamdera.Http

all = EasyTest.run suite

suite :: Test ()
suite = tests $
  [ scope "compile all Elm wire expectations" wire
  ]

wire :: Test ()
wire = do

  failuresM <- io $ newMVar []

  io $ do
    let project = "/Users/mario/dev/projects/elmx/test/scenario-alltypes"

    setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
    setEnv "LTEST" "1"
    setEnv "LDEBUG" "1"
    setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

    let testFiles =
          [
            "src/Test/Wire_Union_1_Basic.elm"
          , "src/Test/Wire_Union_2_Basic.elm"
          , "src/Test/External.elm"
          , "src/Test/Wire_Union_3_Params.elm"
          , "src/Test/Wire_Union_4_Tricky.elm"
          , "src/Test/Wire_Alias_1_Basic.elm"
          , "src/Test/Wire_Alias_2_Record.elm"
          , "src/Test/Wire_Alias_3_SubAlias.elm"
          , "src/Test/Wire_Alias_4_TvarRename.elm"
          , "src/Test/Wire_Tvar_Ambiguous.elm"
          , "src/Test/Wire_Core_Types.elm"
          , "src/Test/Wire_Recursive.elm"
          , "src/Test/Wire_Record_Extensible.elm"
          , "src/Test/Wire_Phantom.elm"
          , "src/Test/Wire_Tvar_Deep.elm"

          ]

    let
      catchTestException :: FilePath -> SomeException -> IO a
      catchTestException filename e = do
        modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
        putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
        throw e


    testFiles & mapM (\filename -> do
        putStrLn $ "testing: " <> show filename
        -- Bust Elm's caching with this one weird trick!
        touch $ project </> filename
        Lamdera.Compile.make project (project </> filename) `catch` catchTestException filename
      )

    unsetEnv "LOVR"
    unsetEnv "LTEST"
    unsetEnv "LDEBUG"
    unsetEnv "ELM_HOME"

  failures <- io $ readMVar failuresM
  if length failures > 0
    then
      crash failures
    else
      scope "senarios-alltypes no exceptions" $ ok



buildAllPackagesRoot = "/Users/mario/lamdera-build-all-packages"

buildAllPackages = do
  mkdir buildAllPackagesRoot

  setEnv "ELM_HOME" "/Users/mario/elm-home-all-packages"
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"

  res <- searchAllCurrentElmPackages

  case res of
    Right packages -> do
      packages
        -- & take 10
        & imapM (\i pkgRaw ->
            case stringToPackageName pkgRaw of
              Just pkg -> do
                putStrLn $ "🧮 " ++ show (i + 1) ++ " of " ++ (show (length packages))
                tryStandaloneInstall pkg
              Nothing ->
                putStrLn $ "⚠️ failed to parse package name: " ++ Text.unpack pkgRaw
          )

      pure ()

    Left err ->
      Lamdera.Http.printHttpError err "I needed to get all current elm packages"

  -- Task.run $
  --   do  env <- Task.eio Exit.InstallBadRegistry $ Solver.initEnv
  --       case env of
  --         Solver.Env cache manager connection latestRegistry ->
  --           case latestRegistry of
  --             Deps.Registry.Registry count versions -> do
  --
  --               versions
  --                 & Map.take 10
  --                 & Map.toList
  --                 & mapM (\(pkg, versions) ->
  --                     tryStandaloneInstall pkg versions
  --                   )
  --                 & Task.io
  --
  --               -- Task.io $ putStrLn $ "total ccount: " ++ show count
  --               -- Task.io $ formatHaskellValue "vals" $
  --
  --       pure ()

  unsetEnv "ELM_HOME"
  unsetEnv "LOVR"


markKnownBad pkg match =
  onlyWhen (textContains match (Text.pack $ Pkg.toChars pkg)) $ writeUtf8 "known_bad" ""

tryStandaloneInstall pkg  = do
  let
    pkgIdent =
      pkg
        & Pkg.toChars
        & Text.pack
        & Text.replace "/" "__"
        & Text.unpack

    root = buildAllPackagesRoot ++ "/" ++ (fmap (\c -> if c == '/' then '_' else c ) pkgIdent)

  mkdir root
  withCurrentDirectory root $ do

    -- Known bad packages that cannot be compiled for non-code reasons
    markKnownBad pkg "Skinney/" -- Author renamed github name
    markKnownBad pkg "2426021684/" -- Renamed
    markKnownBad pkg "ContaSystemer/review-noregex" -- Renamed
    markKnownBad pkg "HAN-ASD-DT/" -- Github not found
    markKnownBad pkg "Morgan-Stanley/morphir-elm" -- renamed finos/morphir-elm
    markKnownBad pkg "abinayasudhir/html-parser" -- tries to call Elm.Kernel.VirtualDom
    markKnownBad pkg "abradley2/" -- 404
    markKnownBad pkg "altjsus/elm-airtable" -- 404, renamed to shegeley/elmtable
    markKnownBad pkg "anatol-1988/measurement" -- 404, renamed to rielas/measurement
    markKnownBad pkg "getto-systems/getto-elm-command" -- renamed, archived
    markKnownBad pkg "jasonliang512/elm-heroicons" -- renamed to jasonliang-dev/elm-heroicons
    markKnownBad pkg "jmg-duarte/group-list" -- "elm-version": "0.17.1 <= v <= 0.19.0",
    markKnownBad pkg "jwheeler-cp/elm-form" -- 404
    markKnownBad pkg "m-mullins/elm-console" -- 404
    markKnownBad pkg "nathanjohnson320/elm-ui-components" -- 404
    markKnownBad pkg "nik-garmash/elm-test" -- 404
    markKnownBad pkg "not1602/elm-feather" -- 404
    markKnownBad pkg "ozyinc/elm-sortable-table-with-row-id" -- 404
    markKnownBad pkg "pascallemerrer/elm-advanced-grid" -- renamed to Orange-OpenSource/elm-advanced-grid
    markKnownBad pkg "proda-ai/elm-logger" -- 404
    markKnownBad pkg "reserve-protocol/elm-i3166-data" -- renamed to reserve-protocol/elm-iso3166-data

    markKnownBad pkg "Cendrb/elm-css" -- Relies on Skinney/murmur3
    markKnownBad pkg "EngageSoftware/elm-engage-common" -- Relies on Skinney/murmur3
    markKnownBad pkg "peterszerzo/elm-natural-ui" -- Relies on Skinney/murmur3
    markKnownBad pkg "the-sett/salix" -- Relies on Skinney/murmur3
    markKnownBad pkg "the-sett/the-sett-laf" -- Relies on Skinney/murmur3

    markKnownBad pkg "altjsus/elmtable" -- SHA changed
    markKnownBad pkg "arsduo/elm-ui-drag-drop" -- SHA changed
    markKnownBad pkg "burnable-tech/elm-ethereum" -- SHA changed
    markKnownBad pkg "chemirea/bulma-classes" -- SHA changed
    markKnownBad pkg "ContaSystemer/review-no-missing-documentation" -- SHA changed
    markKnownBad pkg "doanythingfordethklok/snackbar" -- SHA changed
    markKnownBad pkg "FabienHenon/remote-resource" -- SHA changed
    markKnownBad pkg "JeremyBellows/elm-bootstrapify" -- SHA changed
    markKnownBad pkg "maca/crdt-replicated-graph" -- SHA changed
    markKnownBad pkg "special-elektronik/elm-autocomplete" -- SHA changed
    markKnownBad pkg "tricycle/elm-infnite-gallery" -- SHA changed
    markKnownBad pkg "valentinomicko/test-forms" -- SHA changed
    markKnownBad pkg "waratuman/elm-json-extra" -- SHA changed
    markKnownBad pkg "xdelph/elm-slick-grid" -- SHA changed

    markKnownBad pkg "IzumiSy/elm-consistent-hashing" -- "elm/core": "1.0.4 <= v < 1.0.5",


    markKnownBad pkg "ivadzy/bbase64" -- renamed to "chelovek0v/bbase64"
    markKnownBad pkg "jaredramirez/elm-s3" -- relies on "ivadzy/bbase64"
    markKnownBad pkg "malinoff/elm-jwt" -- relies on "ivadzy/bbase64"


    markKnownBad pkg "lnkr-a/tailwindcss-typed" -- renamed to "laniakea-landscape/tailwindcss-typed"


    -- @TODO issues that might be resolveable
    markKnownBad pkg "robinheghan/elm-deque" -- Has a type Deque a = Deque (Deque (Maybe a)) type which seems inexpressible as an encoder/decoder...?!?!
    markKnownBad pkg "finos/morphir-elm" -- should have worked before?

    markKnownBad pkg "evelios/elm-geometry-quadtree" -- strange exception, when using compiled binary lamdera install works... but strange error when part of this script...
    -- building: evelios/elm-geometry-quadtree
    -- Okay, I created it. Now read that link!
    -- -- CANNOT FIND COMPATIBLE VERSION ------------------------------------- elm.json
    --
    -- I cannot find a version of evelios/elm-geometry-quadtree that is compatible with
    -- your existing dependencies.
    --
    -- I checked all the published versions. When that failed, I tried to find any
    -- compatible combination of these packages, even if it meant changing all your
    -- existing dependencies! That did not work either!
    --
    -- This is most likely to happen when a package is not upgraded yet. Maybe a new
    -- version of Elm came out recently? Maybe a common package was changed recently?
    -- Maybe a better package came along, so there was no need to upgrade this one? Try
    -- asking around https://elm-lang.org/community to learn what might be going on
    -- with this package.
    --
    -- Note: Whatever the case, please be kind to the relevant package authors! Having
    -- friendly interactions with users is great motivation, and conversely, getting
    -- berated by strangers on the internet sucks your soul dry. Furthermore, package
    -- authors are humans with families, friends, jobs, vacations, responsibilities,
    -- goals, etc. They face obstacles outside of their technical work you will never
    -- know about, so please assume the best and try to be patient and supportive!
    --
    -- *** Exception: ExitFailure 1

    -- 🚧🚧🚧🚧 Pending

    markKnownBad pkg "bburdette/cellme" -- looks like wire error, prelude last empty list



    compiledOkay <- doesFileExist "compiled_okay"
    wip <- doesFileExist "wip"
    knownBad <- doesFileExist "known_bad"

    onlyWhen compiledOkay $ putStrLn $ "⏩: skipping already checked: " ++ root
    onlyWhen wip $ putStrLn $ "🔶 skipping project marked wip: " ++ root
    onlyWhen knownBad $ putStrLn $ "❌ skipping known bad project: " ++ root

    onlyWhen (not compiledOkay && not wip && not knownBad) $ do

      putStrLn $ "building: " ++ Pkg.toChars pkg

      hasElmJson <- doesFileExist "elm.json"
      onlyWhen (not hasElmJson) $
        withStdinYesAll $ do
          _ <- Init.init
          pure ()

      installResult <- runInstall (Install.Install pkg) ()
      case installResult of
        Right _ -> do
          -- Package installed + compiled okay!
          -- Write something we can use to skip this on re-runs
          writeUtf8 "compiled_okay" ""
          pure ()

        Left err -> do
          -- putStrLn $ show err
          Exit.toStderr (Exit.installToReport err)
          Exit.exitFailure


stringToPackageName :: Text -> Maybe Pkg.Name
stringToPackageName text =
  case Text.splitOn "/" text of
    author:package:[] ->
      Just $ Pkg.Name (Utf8.fromChars $ Text.unpack author) (Utf8.fromChars $ Text.unpack package)
    _ ->
      Nothing


searchAllCurrentElmPackages :: IO (Either Lamdera.Http.Error [Text])
searchAllCurrentElmPackages = do
  let
    endpoint =
      "https://package.elm-lang.org/search.json"

    decoder :: D.Decoder err [Text]
    decoder =
      D.list (D.field "name" D.text)

  Lamdera.Http.normalJson "fetchAppConfigItems" endpoint decoder



-- Clone of functions from Install with user-prompts removed for headless runs

-- runInstall :: Install.Args -> () -> IO ()
runInstall args () =
  -- Reporting.attempt Exit.installToReport $
    do  maybeRoot <- Stuff.findRoot
        case maybeRoot of
          Nothing ->
            return (Left Exit.InstallNoOutline)

          Just root ->
            case args of
              Install.NoArgs ->
                do  elmHome <- Stuff.getElmHome
                    return (Left (Exit.InstallNoArgs elmHome))

              Install.Install pkg ->
                Task.run $
                  do  env <- Task.eio Exit.InstallBadRegistry $ Solver.initEnv
                      oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root False
                      case oldOutline of
                        Outline.App outline ->
                          do  changes <- Install.makeAppPlan env pkg outline
                              attemptChanges root env oldOutline V.toChars changes

                        Outline.Pkg outline ->
                          do  changes <- Install.makePkgPlan env pkg outline
                              attemptChanges root env oldOutline C.toChars changes


type Task = Task.Task Exit.Install

attemptChanges :: FilePath -> Solver.Env -> Outline.Outline -> (a -> String) -> Install.Changes a -> Task ()
attemptChanges root env oldOutline toChars changes =
  case changes of
    Install.AlreadyInstalled ->
      Task.io $ putStrLn "It is already installed!"

    Install.PromoteIndirect newOutline ->
      attemptChangesHelp root env oldOutline newOutline $
        D.vcat
         [ D.fillSep
            ["I","found","it","in","your","elm.json","file,"
            ,"but","in","the",D.dullyellow "\"indirect\"","dependencies."
            ]
         , D.fillSep
            ["Should","I","move","it","into",D.green "\"direct\""
            ,"dependencies","for","more","general","use?","[Y/n]: "
            ]
         ]

    Install.PromoteTest newOutline ->
      attemptChangesHelp root env oldOutline newOutline $
        D.vcat
         [ D.fillSep
            ["I","found","it","in","your","elm.json","file,"
            ,"but","in","the",D.dullyellow "\"test-dependencies\"","field."
            ]
         , D.fillSep
            ["Should","I","move","it","into",D.green "\"dependencies\""
            ,"for","more","general","use?","[Y/n]: "
            ]
         ]

    Install.Changes changeDict newOutline ->
      let
        widths = Map.foldrWithKey (Install.widen toChars) (Install.Widths 0 0 0) changeDict
        changeDocs = Map.foldrWithKey (Install.addChange toChars widths) (Install.Docs [] [] []) changeDict
      in
      attemptChangesHelp root env oldOutline newOutline $ D.vcat $
        [ "Here is my plan:"
        , Install.viewChangeDocs changeDocs
        , ""
        , "Would you like me to update your elm.json accordingly? [Y/n]: "
        ]


-- attemptChangesHelp :: FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> D.Doc -> Task ()
attemptChangesHelp root env oldOutline newOutline question =
  Task.eio Exit.InstallBadDetails $
  BW.withScope $ \scope ->
  do  Outline.write root newOutline
      result <- Details.verifyInstall scope root env newOutline
      case result of
        Left exit ->
          do  Outline.write root oldOutline
              return (Left exit)

        Right () ->
          do  putStrLn "Success!"
              return (Right ())
