{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Project where

import qualified Data.Text as T
import System.Process (readProcess)
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Environment as Env
import qualified Data.List as List

import Lamdera


inProduction :: IO Bool
inProduction = do
  appNameEnvM <- liftIO $ Env.lookupEnv "LAMDERA_APP_NAME"
  forceNotProd <- liftIO $ Env.lookupEnv "NOTPROD"
  pure $ (appNameEnvM /= Nothing && forceNotProd == Nothing) -- @TODO better isProd check...


maybeAppName :: IO (Maybe Text)
maybeAppName = do

  appNameEnvM <- liftIO $ Env.lookupEnv "LAMDERA_APP_NAME"
  lamderaRemotes <- liftIO getLamderaRemotes

  if (lamderaRemotes == [] && appNameEnvM == Nothing)
    then
      pure Nothing
    else
      -- Prior `if` guards against situation where no name is determinable
      pure $ Just $ certainAppName lamderaRemotes appNameEnvM


certainAppName :: [Text] -> Maybe String -> Text
certainAppName lamderaRemotes appNameEnvM =
  case appNameEnvM of
    Just appNameEnv -> T.pack appNameEnv
    _ ->
      List.head lamderaRemotes
        & T.splitOn ":"
        & (\l ->
            case l of
              f:second:_ -> second
          )
        & T.splitOn "."
        & List.head


getLamderaRemotes :: IO [Text]
getLamderaRemotes = do
  gitRemotes <- readProcess "git" ["remote", "-v"] ""
  gitRemotes
    & T.pack
    & T.splitOn "\n"
    & filter (textContains "apps.lamdera.com")
    & filter (textContains "(push)")
    & pure


lamderaThrowUnknownApp :: Task.Task ()
lamderaThrowUnknownApp =
  Task.throw $ Exit.Lamdera
    $ Help.report "UNKNOWN APP" (Just "git remote -v")
      ("I cannot figure out which Lamdera app this repository belongs to!")
      ([ D.reflow "I normally look for a git remote called 'lamdera' but did not find one."
       , D.reflow "Did you maybe forget to add the lamdera remote for your app as listed on the Dashboard?"
       , D.reflow "See <https://dashboard.lamdera.app/docs/deploying> for more info."
       ]
      )
