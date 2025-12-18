{-# LANGUAGE TemplateHaskell #-}
module Lamdera.CLI.Backend
  ( Flags(..)
  , run
  )
  where


import Control.Applicative ((<|>))
import System.FilePath ((</>))

import qualified BackgroundWriter as BW
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.FileEmbed as FE
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as TE
import qualified Language.Haskell.TH as TH
import qualified System.Directory as Dir
import qualified System.Exit
import qualified System.IO as IO
import qualified System.Process as Proc

import qualified Build
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task
import qualified Stuff

import qualified Lamdera
import qualified Lamdera.Relative



-- RUN


data Flags =
  Flags
    { _eval :: Maybe String
    , _import :: Maybe String
    , _interpeter :: Maybe String
    }


run :: () -> Flags -> IO ()
run _ (Flags maybeEval maybeImport maybeAlternateInterpreter) = do
  interpreter <- getInterpreter maybeAlternateInterpreter
  maybeRoot <- Stuff.findRoot
  exitCode  <- Reporting.attempt Exit.makeToReport $
    case maybeRoot of
      Just root -> runHelp interpreter root maybeImport maybeEval
      Nothing   -> return $ Left $ Exit.MakeNoOutline
  System.Exit.exitWith exitCode


runHelp :: FilePath -> FilePath -> Maybe String -> Maybe String -> IO (Either Exit.Make System.Exit.ExitCode)
runHelp interpreter root maybeImport maybeEval = do
  let path = Lamdera.lamderaCache root </> "Backend_Eval_.elm"

  writeBackendEvalFile path maybeImport (Maybe.fromMaybe "model" maybeEval)
  javaScript <- compileBackendEvalCode root path

  case executeBackendEvalCode interpreter <$> javaScript of
    Left err -> return $ Left err
    Right m  -> Right <$> m



-- WRITE


writeBackendEvalFile :: FilePath -> Maybe String -> String -> IO ()
writeBackendEvalFile path maybeImport expression =
  let
    codeLines = BS.lines backendEvalCode
    firstCodeLines = take 4 codeLines
    remainingCodeLines = drop 4 codeLines

    importLines = BS.pack <$> lines (Maybe.fromMaybe "" maybeImport)
    expressionLines = BS.pack <$> lines (" " ++ expression)

    patchedCode = BS.unlines $
      firstCodeLines
      ++ importLines
      ++ reverse (drop 1 (reverse remainingCodeLines))
      ++ expressionLines
  in
  Lamdera.writeUtf8 path (TE.decodeUtf8 patchedCode)


backendEvalCode :: BS.ByteString
backendEvalCode =
  $(FE.bsToExp =<< TH.runIO (Lamdera.Relative.readByteString "extra/LocalDev/backend-src/Backend_Eval_.elm"))



-- COMPILE


compileBackendEvalCode :: FilePath -> FilePath -> IO (Either Exit.Make B.Builder)
compileBackendEvalCode root path =
  BW.withScope $ \scope -> do
    Task.run $ do
      details   <- Task.eio Exit.MakeBadDetails $ Details.load Reporting.silent scope root
      artifacts <- Task.eio Exit.MakeCannotBuild $ Build.fromPaths Reporting.silent root details $ NE.singleton path
      Task.mapError Exit.MakeBadGenerate $ Generate.dev root details artifacts



-- EXECUTE


executeBackendEvalCode :: FilePath -> B.Builder -> IO System.Exit.ExitCode
executeBackendEvalCode interpreter jsBuilder = do
  let finalCode = jsBuilder <> B.byteString backendEvalInitCode
  -- Lamdera.writeUtf8 "backend-eval.js" (TE.decodeUtf8 $ LBS.toStrict $ B.toLazyByteString finalCode)
  interpret interpreter finalCode


backendEvalInitCode :: BS.ByteString
backendEvalInitCode =
  $(FE.bsToExp =<< TH.runIO (Lamdera.Relative.readByteString "extra/LocalDev/backend-src/init-backend-eval.js"))


interpret :: FilePath -> B.Builder -> IO System.Exit.ExitCode
interpret interpreter javascript =
  let
    createProcess = (Proc.proc interpreter []) { Proc.std_in = Proc.CreatePipe }
  in
  Proc.withCreateProcess createProcess $ \(Just stdin) _ _ handle ->
    do  B.hPutBuilder stdin javascript
        IO.hClose stdin
        Proc.waitForProcess handle



-- GET INTERPRETER


getInterpreter :: Maybe String -> IO FilePath
getInterpreter maybeName =
  case maybeName of
    Just name ->
      getInterpreterHelp name (Dir.findExecutable name)

    Nothing ->
      getInterpreterHelp "node` or `nodejs" $
        do  exe1 <- Dir.findExecutable "node"
            exe2 <- Dir.findExecutable "nodejs"
            return (exe1 <|> exe2)


getInterpreterHelp :: String -> IO (Maybe FilePath) -> IO FilePath
getInterpreterHelp name findExe =
  do  maybePath <- findExe
      case maybePath of
        Just path ->
          return path

        Nothing ->
          do  IO.hPutStrLn IO.stderr (exeNotFound name)
              System.Exit.exitFailure


exeNotFound :: String -> String
exeNotFound name =
  "'lamdera backend' relies on node.js to execute JavaScript code outside the browser.\n"
  ++ "I could not find executable `" ++ name ++ "` on your PATH though!\n\n"
  ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
  ++ "but has a different name, use the --interpreter flag."
