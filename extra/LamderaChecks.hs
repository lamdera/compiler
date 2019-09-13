{-# LANGUAGE OverloadedStrings #-}

module LamderaChecks (runChecks) where

import Data.Text as T
import Data.Text.IO as TIO
import qualified System.Directory as Dir
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT, liftIO)
import Data.Monoid ((<>))

type Check = ExceptT Text IO

runChecks = do
  r <- runExceptT checks
  case r of
    Right _ -> pure True

    Left err -> do
      TIO.putStrLn err
      pure False


checks :: Check Bool
checks = do
  missingFiles <- liftIO $ checkMissingFiles ["src/Frontend.elm", "src/Backend.elm", "src/Msg.elm"]
  unless (missingFiles == []) $
    throwError $
      "The following files required by Lamdera are missing:\n"
      <>
      (T.concat $ fmap (\file -> "\n- " <> T.pack file) missingFiles)
      <>
      "\n\nPlease see https://alpha.lamdera.app/development for how to set up a Lamdera app."


  frontendAppDefined <- liftIO $ checkHasAppDefinition "src/Frontend.elm"
  unless frontendAppDefined $
    throwError
      "src/Frontend.elm is missing an `app` definition. \n\
      \\n\
      \Lamdera apps need an `app =` definition in Frontend.elm and Backend.elm so I know where the app begins, similar to how Elm apps need a `main`.\n\
      \\n\
      \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."


  backendAppDefined <- liftIO $ checkHasAppDefinition "src/Backend.elm"
  unless backendAppDefined $
    throwError
      "src/Backend.elm is missing an `app` definition. \n\
      \\n\
      \Lamdera apps need an `app =` definition in Frontend.elm and Backend.elm so I know where the app begins, similar to how Elm apps need a `main`.\n\
      \\n\
      \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."


  msgHasTypes <- liftIO $ checkMsgHasTypes ["FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]
  unless (msgHasTypes == True) $
    throwError
      "src/Msg.elm is missing some type definitions.\n\
      \\n\
      \Lamdera apps need FrontendMsg, ToBackend, BackendMsg and ToFrontend types defined in Msg.elm.\n\
      \\n\
      \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."


  pure True


checkMissingFiles :: [FilePath] -> IO [FilePath]
checkMissingFiles paths = do
  exists <- mapM (\path -> do
    exist <- Dir.doesFileExist path
    if exist then pure (True, path) else pure (False, path)
    ) paths

  let
    missingFilePairs =
      Prelude.filter (\(exists, path) -> exists == False) exists

    missingFilePaths =
      Prelude.map (\(exists, path) -> path) missingFilePairs

  pure missingFilePaths


checkHasAppDefinition :: FilePath -> IO Bool
checkHasAppDefinition path = do
  source <- TIO.readFile path
  pure $ T.isInfixOf "app =" source


checkMsgHasTypes :: [Text] -> IO Bool
checkMsgHasTypes typeNames = do
  source <- TIO.readFile "src/Msg.elm"

  let
    results = fmap (\search -> T.isInfixOf ("type " <> search) source) typeNames

  pure $ Prelude.all ((==) True) results
