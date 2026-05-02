{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Repl (serve) where

import qualified BackgroundWriter as BW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.POSIX as P
import qualified Language.Haskell.TH as TH
import qualified Snap.Core as S
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Generate
import qualified Json.Encode as JE
import qualified Json.String as JS
import qualified Reporting
import qualified Reporting.Task as Task
import qualified Stuff

import qualified Lamdera
import qualified Ext.Common
import qualified Lamdera.Version



-- WEB SERVER


serve :: FilePath -> (B.Builder -> S.Snap ()) -> (B.Builder -> S.Snap ()) -> S.Snap ()
serve root jsonResponse error404 = do
  request <- S.getRequest
  let path = TE.decodeUtf8 $ S.rqPathInfo request
  if path == "_repl-worker.js"
    then do
      S.modifyResponse (S.setContentType "text/javascript;charset=utf-8")
      S.writeBS Lamdera.Repl.lamderaReplSrc
    else
      serveFiles root jsonResponse error404 path



-- BUILD REPL WORKER


lamderaReplSrc :: BS.ByteString
lamderaReplSrc = $(do

  let mainPaths = NE.singleton "src/Repl/Worker.elm"

  compilerResult <- TH.runIO $ do
    putStr "-- Compiling `src/Repl/Worker.elm` in `vendor/elm-repl-worker`..."
    BW.withScope $ \scope -> do
      Dir.withCurrentDirectory ("vendor" </> "elm-repl-worker") $ do
        root <- Dir.getCurrentDirectory
        Lamdera.withProjectRoot root $ do
          Task.run $ do
            details    <- Task.eio (const "details") $ Details.load Reporting.silent scope root
            artifacts  <- Task.eio (const "build") $ Build.fromPaths Reporting.silent root details mainPaths
            javascript <- Task.mapError (const "generate") $ Generate.dev root details artifacts
            return $ LBS.toStrict (B.toLazyByteString javascript)

  compiledCode <-
    case compilerResult of
      Right compiledCode_ -> do
        TH.runIO $ putStrLn " OK"
        return compiledCode_
      Left err ->
        error $
          "\nError in Lamdera.Repl.lamderaRepl during the compiler " ++ err ++ " phase.\
          \\n  Try running `lamdera make src/Repl/Worker.elm` directly."

  minifiedCode <- TH.runIO $ do
    isGithubActions <- Lamdera.lookupEnv "GITHUB_ACTIONS"
    let tempFile = "extra" </> "repl-worker.js"
    let distFile = "extra" </> "dist" </> "repl-worker.js"
    case isGithubActions of
      Just "true" -> do
        putStr $ "-- Loading pre-built `" <> distFile <> "`..."
        exists <- Dir.doesFileExist distFile
        if exists
          then do
            putStrLn " OK"
            BS.readFile distFile
          else
            error $
              "\nError in Lamdera.Repl.lamderaRepl: pre-built repl-worker.js not found.\
              \\n  Expected file at: " <> distFile <> "\
              \\n  Run `stack install` locally to build and commit to the repository."
      _ -> do
        putStr $ "-- Minifying `" <> tempFile <> "`..."
        _ <- Ext.Common.requireBinary "esbuild"
        BS.writeFile tempFile compiledCode
        Lamdera.replaceInFile "artifacts.x.dat" (T.pack Lamdera.Version.artifacts) tempFile
        let esbuildCommand = "esbuild " <> tempFile <> " --minify --target=chrome58,firefox57,safari11,edge16 > " <> distFile
        _ <- Ext.Common.bash esbuildCommand
        minifierResult <- Dir.doesFileExist distFile
        if minifierResult
          then do
            putStrLn " OK"
            minifiedCode_ <- BS.readFile distFile
            Dir.removeFile tempFile
            return minifiedCode_
          else
            error $
              "\nError in Lamdera.Repl.lamderaRepl during minification.\
              \\n  Try running `" <> esbuildCommand <> "` directly."

  Data.FileEmbed.bsToExp minifiedCode)



-- SERVE FILES


serveFiles :: FilePath -> (B.Builder -> S.Snap ()) -> (B.Builder -> S.Snap ()) -> T.Text -> S.Snap ()
serveFiles root jsonResponse error404 path = do
  elmHome <- Lamdera.liftIO $ Stuff.getElmHome
  let
    fullpath :: FilePath
    fullpath
      | path == "~/.elm"            = elmHome
      | T.isPrefixOf "~/.elm/" path = elmHome </> T.unpack (T.drop 7 path)
      | otherwise                   = root </> T.unpack path
  exists_ <- Lamdera.liftIO $ Dir.doesPathExist fullpath
  if exists_
    then do
      isDir_ <- Lamdera.liftIO $ Dir.doesDirectoryExist fullpath
      if isDir_
        then do
          dir <- Lamdera.liftIO $ serveDir fullpath
          jsonResponse $ JE.encode dir
        else do
          S.sendFile fullpath
    else do
      error404 "path not found"


serveDir :: FilePath -> IO JE.Value
serveDir path = do
  dir <- serveDirHelper path
  mTime <- getMTime path
  return $ JE.array [ dir, mTime ]


serveDirHelper :: FilePath -> IO JE.Value
serveDirHelper path = do
  files <- Dir.listDirectory path
  fields <-
    mapM
      (\entry -> do
        let entryPath = path </> entry
        isDir <- Dir.doesDirectoryExist entryPath
        value <-
          if isDir
            then serveDirHelper entryPath
            else JE.int . fromInteger <$> Dir.getFileSize entryPath
        mTime <- getMTime entryPath
        return ( JS.fromChars entry, JE.array [ value, mTime ] )
      )
      files
  return $ JE.object fields


getMTime :: FilePath -> IO JE.Value
getMTime path = do
  mtime <- Dir.getModificationTime path
  return $ JE.int $ truncate (P.utcTimeToPOSIXSeconds mtime * 1000)
