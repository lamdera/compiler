{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Project where

{- Project helper functions
-}

import qualified Data.Text as T
import Data.Char (toLower, isAlphaNum)
import System.Process (readProcess)
import System.Exit (exitFailure)

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified System.Environment as Env
import qualified Data.List as List
import qualified Data.Utf8 as Utf8
import Elm.Package

import Data.Word (Word16)

import Lamdera
import qualified Lamdera.Progress


maybeAppName :: IO (Maybe Text)
maybeAppName = do
  appNameEnvM <- Env.lookupEnv "LAMDERA_APP_NAME"
  lamderaRemotes <- getLamderaRemotes

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
    & filter (\t -> textContains "apps.lamdera.com" t || textHasPrefix "lamdera" t)
    & filter (textContains "(push)")
    & pure


lamderaThrowUnknownApp :: IO ()
lamderaThrowUnknownApp =
  Lamdera.Progress.throw lamderaUnknownApp


makeNameClean :: T.Text -> T.Text
makeNameClean appName =
    T.take 20 . T.toLower . T.filter isAllowed $ appName
  where
    isAllowed c = isAlphaNum c || c == '-'


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
  Elm.Package.Name (Utf8.fromChars "lamdera") (Utf8.fromChars "core")


{-# NOINLINE lamderaCodecs #-}
lamderaCodecs :: Elm.Package.Name
lamderaCodecs =
  Elm.Package.Name (Utf8.fromChars "lamdera") (Utf8.fromChars "codecs")


{-# NOINLINE lamderaProgramTest #-}
lamderaProgramTest :: Elm.Package.Name
lamderaProgramTest =
  Elm.Package.Name (Utf8.fromChars "lamdera") (Utf8.fromChars "program-test")


{-# NOINLINE lamderaWebsocket #-}
lamderaWebsocket :: Elm.Package.Name
lamderaWebsocket =
  Elm.Package.Name (Utf8.fromChars "lamdera") (Utf8.fromChars "websocket")


findOverridePackages :: IO [(Elm.Package.Name, Word16, Word16, Word16)]
findOverridePackages =
  -- @STUB todo: make this actually find override packages dynamically and draw in their deps
  -- this would allow us to have private unpublished local packages in LOVR that are auto added
  -- in addition to the existing package override behaviour
  -- pure [(Elm.Package.Name (Utf8.fromChars "lamdera") (Utf8.fromChars "websocket"), 1,0,0)]
  pure []
