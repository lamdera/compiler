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
  [ scope "compile all Elm wire expectations" $ do
      io wire
  ]

wire :: IO ()
wire = do
  let project = "/Users/mario/dev/projects/elmx/test"
  -- let project = "/Users/mario/lamdera/test/v1"
  -- let project = "/Users/mario/dev/projects/elmx/test/architecture-test"
  -- let project = "/Users/mario/dev/projects/elmx/test/elm-units"
  -- let project = "/Users/mario/dev/projects/lamdera-dashboard"

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

  testFiles & mapM (\filename -> do
      -- Bust Elm's caching with this one weird trick!
      touch $ project </> filename
      Lamdera.Compile.make project (project </> filename)
    )

  unsetEnv "LOVR"
  unsetEnv "LTEST"
  unsetEnv "LDEBUG"
  unsetEnv "ELM_HOME"


buildAllPackagesRoot = "/Users/mario/lamdera-build-all-packages"

buildAllPackages = do
  mkdir buildAllPackagesRoot

  setEnv "ELM_HOME" "/Users/mario/elm-home-all-packages"

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


matchKnownBad pkg match =
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
    matchKnownBad pkg "Skinney/" -- Author renamed github name
    matchKnownBad pkg "2426021684/" -- Renamed
    matchKnownBad pkg "ContaSystemer/review-noregex" -- Renamed
    matchKnownBad pkg "HAN-ASD-DT/" -- Github not found
    matchKnownBad pkg "Morgan-Stanley/morphir-elm" -- renamed finos/morphir-elm
    matchKnownBad pkg "abinayasudhir/html-parser" -- tries to call Elm.Kernel.VirtualDom
    matchKnownBad pkg "abradley2/" -- 404
    matchKnownBad pkg "altjsus/elm-airtable" -- 404, renamed to shegeley/elmtable
    matchKnownBad pkg "anatol-1988/measurement" -- 404, renamed to rielas/measurement
    matchKnownBad pkg "getto-systems/getto-elm-command" -- renamed, archived
    matchKnownBad pkg "jasonliang512/elm-heroicons" -- renamed to jasonliang-dev/elm-heroicons
    matchKnownBad pkg "jmg-duarte/group-list" -- "elm-version": "0.17.1 <= v <= 0.19.0",
    matchKnownBad pkg "jwheeler-cp/elm-form" -- 404
    matchKnownBad pkg "m-mullins/elm-console" -- 404
    matchKnownBad pkg "nathanjohnson320/elm-ui-components" -- 404
    matchKnownBad pkg "nik-garmash/elm-test" -- 404
    matchKnownBad pkg "not1602/elm-feather" -- 404
    matchKnownBad pkg "ozyinc/elm-sortable-table-with-row-id" -- 404
    matchKnownBad pkg "pascallemerrer/elm-advanced-grid" -- renamed to Orange-OpenSource/elm-advanced-grid
    matchKnownBad pkg "proda-ai/elm-logger" -- 404
    matchKnownBad pkg "reserve-protocol/elm-i3166-data" -- renamed to reserve-protocol/elm-iso3166-data

    matchKnownBad pkg "Cendrb/elm-css" -- Relies on Skinney/murmur3
    matchKnownBad pkg "EngageSoftware/elm-engage-common" -- Relies on Skinney/murmur3
    matchKnownBad pkg "peterszerzo/elm-natural-ui" -- Relies on Skinney/murmur3
    matchKnownBad pkg "the-sett/salix" -- Relies on Skinney/murmur3
    matchKnownBad pkg "the-sett/the-sett-laf" -- Relies on Skinney/murmur3

    matchKnownBad pkg "altjsus/elmtable" -- SHA changed
    matchKnownBad pkg "arsduo/elm-ui-drag-drop" -- SHA changed
    matchKnownBad pkg "burnable-tech/elm-ethereum" -- SHA changed
    matchKnownBad pkg "chemirea/bulma-classes" -- SHA changed
    matchKnownBad pkg "ContaSystemer/review-no-missing-documentation" -- SHA changed
    matchKnownBad pkg "doanythingfordethklok/snackbar" -- SHA changed
    matchKnownBad pkg "FabienHenon/remote-resource" -- SHA changed
    matchKnownBad pkg "JeremyBellows/elm-bootstrapify" -- SHA changed
    matchKnownBad pkg "maca/crdt-replicated-graph" -- SHA changed
    matchKnownBad pkg "special-elektronik/elm-autocomplete" -- SHA changed
    matchKnownBad pkg "tricycle/elm-infnite-gallery" -- SHA changed
    matchKnownBad pkg "valentinomicko/test-forms" -- SHA changed
    matchKnownBad pkg "waratuman/elm-json-extra" -- SHA changed
    matchKnownBad pkg "xdelph/elm-slick-grid" -- SHA changed


    -- @TODO issues that might be resolveable
    matchKnownBad pkg "robinheghan/elm-deque" -- Has a type Deque a = Deque (Deque (Maybe a)) type which seems inexpressible as an encoder/decoder...?!?!



    matchKnownBad pkg "evelios/elm-geometry-quadtree" -- strange exception, when using compiled binary lamdera install works... but strange error when part of this script...
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

    -- Pending



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
