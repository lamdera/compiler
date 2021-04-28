{-# LANGUAGE OverloadedStrings #-}

module LamderaSharedBuildHelpers where

import System.Process (readProcessWithExitCode)
import qualified System.Directory as Dir
import qualified Data.Text as T
import Lamdera

adminToken = "ntil6p9l4i1zylcreuisd0ncrf17xxko"

clearPriorBuilds = do
  c "rm -rf ~/lamdera-builds/build-test-local || true"

clearPriorDeploys = do
  c "rm -rf ~/lamdera-deploys/test-local-v* || true"

clearSnapshots = do
  c "rm -rf ~/lamdera-snapshots/test-local || true"

clearBuildCache projectPath =
  c $ "rm -rf " <> projectPath <> "/elm-stuff || true"


rebootTestBuildServices = do
  putStrLn "➡️  Rebooting test build services..."
  rebootConfigurator
  c "~/lamdera/logging/reboot.sh"
  c "~/dev/projects/lamdera-dashboard/reboot.sh"
  pure ()

rebootConfigurator = c "~/lamdera/configurator/reboot.sh"


copyRuntimeFiles projectPath = do
  c $ "cp -rp ~/lamdera/runtime/src/* " <> projectPath <> "/src/"
  c $ "mkdir -p " <> projectPath <> "/js/"
  c $ "cp -rp ~/lamdera/runtime/js/* " <> projectPath <> "/js/"
  c $ "cp -rp ~/lamdera/runtime/package.json " <> projectPath <> "/package.json"
  c $ "cp -rp ~/lamdera/runtime/package-lock.json " <> projectPath <> "/package-lock.json"
  c $ "cp -rp ~/lamdera/runtime/backend.js " <> projectPath <> "/backend.js"
  c $ "cp -rp ~/lamdera/runtime/oracle.js " <> projectPath <> "/oracle.js"
  c $ "cp -rp ~/lamdera/runtime/frontend.js " <> projectPath <> "/frontend.js"
  c $ "cp -rp ~/lamdera/runtime/index.html " <> projectPath <> "/index.html"

injectElmPkgJsIncludesDefaultIfMissing projectPath = do
  pkgJsIncludesExists <- Dir.doesFileExist $ projectPath <> "/elm-pkg-js-includes.js"
  if pkgJsIncludesExists
    then
      -- No problem
      pure ()
    else
      -- No includes file, add the default
      c $ "cp -rp ~/lamdera/runtime/elm-pkg-js-includes-empty.js " <> projectPath <> "/elm-pkg-js-includes.js"


injectFrontendAppConfig projectPath appName version = do
  replaceInFile
    "/*INLINE_LAMDERA_CONFIG*/"
    (T.pack $ "window['lamdera']['conf'] = { appId: '" <> appName <> "', appVersion: " <> version <> " }" )
    (projectPath <> "/frontend.js")

npmInstall projectPath = do
  c $ "cd " <> projectPath <> " && npm i --production"

killPm2Instances appName = do
  c $ "pm2 delete " <> appName <> " || true"
  c $ "pm2 delete " <> appName <> "-zero || true"

addRPCDefaultIfMissing projectRoot = do
  rpcExists <- Dir.doesFileExist $ projectRoot <> "/src/RPC.elm"
  if rpcExists
    then
      -- No problem
      c $ "rm " <> projectRoot <> "/src/RPC_Empty.elm"
    else
      -- No RPC implementation, set the default empty one
      c $ "cp " <> projectRoot <> "/src/RPC_Empty.elm " <> projectRoot <> "/src/RPC.elm"



parcelFrontendNoMinify projectPath = do
  c $ "cd " <> projectPath <> " && parcel build index.html --no-minify --no-source-maps --cache-dir ../cache/parcel"

parcelFrontendMinify projectPath = do
  c $ "cd " <> projectPath <> " && parcel build index.html --no-source-maps --cache-dir ../cache/parcel"

clearEnv = do
  unsetEnv "FORCEVERSION"
  unsetEnv "LDEBUG"
  unsetEnv "TOKEN"
  unsetEnv "LOVR"
  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "ELM_HOME"

bootNodejsApp projectPath appName = do
  c $ "cd " <> projectPath <> " && APPNAME=" <> appName <> " node --inspect --max-old-space-size=3072 --expose-gc oracle.js"

c = callCommand
