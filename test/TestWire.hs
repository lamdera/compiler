{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module TestWire where

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
import Lamdera.Snapshot
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
        , "src/Test/Wire_Core_Types.elm"
        , "src/Test/Wire_Recursive.elm"
        , "src/Test/Wire_Phantom.elm"
        -- "src/Types.elm"
        -- "src/ArchitectureTest.elm"
         -- "src/Acceleration.elm"
        -- "src/Codec.elm"
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
    -- /  putStrLn $ show $ take 10 packages

      packages
        -- & take 10
        & mapM (\pkgRaw ->
            case stringToPackageName pkgRaw of
              Just pkg ->
                tryStandaloneInstall pkg
              Nothing ->
                putStrLn $ "⚠️ failed to parse package name: " ++ Text.unpack pkgRaw
          )

      pure ()

    Left err ->
      putStrLn $ Lamdera.Http.errorToString err

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

    compiledOkay <- doesFileExist "compiled_okay"
    wip <- doesFileExist "wip"

    if compiledOkay
      then
        putStrLn $ "⏩: skipping already checked: " ++ root

      else do

        if wip
          then
            putStrLn $ "🔶 skipping project marked wip: " ++ root

          else do

            putStrLn $ "building: " ++ root

            hasElmJson <- doesFileExist "elm.json"
            onlyWhen (not hasElmJson) $
              withStdinYesAll $ do
                _ <- Init.init
                pure ()

            installResult <- runInstall (Install.Install pkg) ()
            case installResult of
              Right _ -> do
                -- @TODO is this needed? Does install also build the package...?
                mkdir "src"
                writeUtf8 "src/Test.elm" "module Test exposing (..)\n\nx = 1\n"
                Lamdera.Compile.make "src" "Test.elm"

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
                      oldOutline <- Task.eio Exit.InstallBadOutline $ Outline.read root
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
