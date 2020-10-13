{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Project where

{- Project helper functions
-}

import qualified Data.Text as T
import System.Process (readProcess)
import System.Exit (exitFailure)
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Environment as Env
import qualified Data.List as List
import Elm.Package

import Lamdera
import qualified Lamdera.Progress
import StandaloneInstances



shouldHaveCodecsGenerated :: Elm.Package.Name -> Bool
shouldHaveCodecsGenerated name =
  case name of
    -- Some elm packages are ignored because of cyclic dependencies.
    -- Those codecs have to be manually defined in `lamdera/codecs`.
    -- All other packages, even if their types are defined in js, have codecs generated for their types.
    -- Then we manually override specific types in `Wire.Source`.

    -- elm deps used by lamdera/codecs
    Name "elm" "bytes" -> False
    Name "elm" "core" -> False

    -- avoid cyclic imports; generated codecs rely on lamdera/codecs:Lamdera.Wire. This is our codec bootstrap module.
    Name "lamdera" "codecs" -> False

    -- Everything else should have codecs generated
    -- _ -> True
    Name "elm" "time" -> True -- @TODO REMOVE
    Name "author" "project" -> True -- @TODO REMOVE
    _ -> False -- @TODO REMOVE



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


appNameOrThrow :: IO Text
appNameOrThrow = do
  appNameM <- maybeAppName

  case appNameM of
    Just appName -> do
      pure appName

    Nothing -> do
      Lamdera.Progress.report $ Help.reportToDoc lamderaUnknownApp
      exitFailure


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


lamderaThrowUnknownApp :: IO ()
lamderaThrowUnknownApp =
  Lamdera.Progress.throw lamderaUnknownApp


lamderaUnknownApp =
  Help.report "UNKNOWN APP" (Just "git remote -v")
    ("I cannot figure out which Lamdera app this repository belongs to!")
    ([ D.reflow "I normally look for a git remote called `lamdera` but did not find one."
     , D.reflow "Did you add the `lamdera` remote for your app as listed on the Dashboard?"
     , D.reflow "See <https://dashboard.lamdera.app/docs/deploying> for more info."
     ]
    )


{-# NOINLINE lamderaCore #-}
lamderaCore :: Elm.Package.Name
lamderaCore =
  Elm.Package.Name "lamdera" "core"


{-# NOINLINE lamderaCodecs #-}
lamderaCodecs :: Elm.Package.Name
lamderaCodecs =
  Elm.Package.Name "lamdera" "codecs"
