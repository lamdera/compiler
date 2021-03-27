{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Checks where

import qualified Data.Map as Map
import Data.Text as T
import Data.Text.IO as TIO
import qualified System.Directory as Dir
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT, liftIO)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import qualified Debug.Trace as DT
import qualified Data.List as List

import qualified Reporting.Doc as D
import qualified Elm.Package as Pkg
import Elm.ModuleName (Canonical(..))
import qualified Type.Error as T
import qualified Reporting
import qualified Reporting.Task as Task
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Elm.Version as V

import NeatInterpolation

import Lamdera
import Lamdera.Progress


runChecks :: Bool -> Map.Map Pkg.Name V.Version -> IO (Either Exit.Outline outline) -> IO (Either Exit.Outline outline)
runChecks shouldCheckLamdera direct default_ = do
  -- atomicPutStrLn $ "runchecks but with " <> show shouldCheckLamdera
  if Map.member Pkg.lamderaCore direct
    then do
      onlyWhen shouldCheckLamdera Lamdera.Checks.runChecks_
      default_
    else
      if shouldCheckLamdera
        then return $ Left Exit.OutlineLamderaMissingDeps
        else default_


runChecks_ :: IO ()
runChecks_ = do

  missingFiles <- liftIO $ checkMissingFiles ["src/Frontend.elm", "src/Backend.elm", "src/Types.elm"]
  unless (missingFiles == []) $ do

    let
      formattedErrors = missingFiles & fmap (\file -> D.reflow $ "- " <> file )

    if List.length missingFiles == 3
      then do

      initialiseLamderaFiles <- Reporting.ask $
        Help.reportToDoc $
          Help.report "MISSING FILES" (Nothing)
            ("The following files required by Lamdera are missing:")
            (formattedErrors ++
            [ D.reflow $ "It looks like you're starting from scratch!"
            , D.reflow $ "Would you like me to create a starter implementation? [Y/n]: "
            ])

      if initialiseLamderaFiles
        then do
          liftIO $ writeDefaultImplementations
          -- @TODO future
          -- It would be nice if when coming from an existing elm project, we installed the missing
          -- deps as well, but the UI impact is a little bit weird as it's not transparent to the user
          -- whats happening, and at the same time the runInstall headless helper we have causes cyclic
          -- deps so can't really use that until we unravel things...
          -- liftIO $ callCommand "elm install elm/url"
          -- This would be much better but we have cyclic dep issues
          -- Install.run (Install.Install (Package.Name.toName Package.Name.elm "url")) ()
          --
          -- Not a huge deal though, the user error explains that elm/url is missing so overall
          -- users should be able to unblock albeit it being a bit ugly a process.

          report $ D.green "Okay, I've generated them for you!\n"
        else
          throw
            $ Help.report "SKIPPING AUTO-GENERATION" (Nothing)
              ("Okay, I'll let you implement them!")
              [ D.reflow "See <https://dashboard.lamdera.app/docs/building> for more."]

      else

        throw
          $ Help.report "MISSING FILES" (Nothing)
              ("The following files required by Lamdera are missing:")
              (formattedErrors ++
              [ D.reflow "It looks like you've already started implementing things!"
              , D.reflow "See <https://dashboard.lamdera.app/docs/building> for more."
              ])


  -- Ensure Env.elm is in place before we get going, otherwise
  -- our `mode` value injection will fail spectacularly
  envExists <- liftIO $ doesFileExist "src/Env.elm"

  onlyWhen (not envExists) $ do
    liftIO $ writeUtf8 "src/Env.elm" emptyEnv
    progressPointer "Created empty Env.elm"


progressPointer t =
    report $ D.fillSep [ D.fromChars "───>", D.green $ t <> "\n" ]



writeDefaultImplementations = do
  root <- getProjectRoot
  defaultImplementations
    & mapM (\(filename, implementation) ->
        writeUtf8 (root </> filename) implementation
      )
  writeLineIfMissing "elm-stuff" (root </> ".gitignore")


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
  source <- TIO.readFile "src/Types.elm"

  let
    results = fmap (\search -> T.isInfixOf ("type " <> search) source) typeNames

  pure $ Prelude.all ((==) True) results



defaultImplementations :: [(FilePath, Text)]
defaultImplementations =
  [ ("src/Frontend.elm", [text|
      module Frontend exposing (..)

      import Browser exposing (UrlRequest(..))
      import Browser.Navigation as Nav
      import Html
      import Html.Attributes as Attr
      import Lamdera
      import Types exposing (..)
      import Url


      type alias Model =
          FrontendModel


      app =
          Lamdera.frontend
              { init = init
              , onUrlRequest = UrlClicked
              , onUrlChange = UrlChanged
              , update = update
              , updateFromBackend = updateFromBackend
              , subscriptions = \m -> Sub.none
              , view = view
              }


      init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
      init url key =
          ( { key = key
            , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
            }
          , Cmd.none
          )


      update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
      update msg model =
          case msg of
              UrlClicked urlRequest ->
                  case urlRequest of
                      Internal url ->
                          ( model
                          , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                          )

                      External url ->
                          ( model
                          , Nav.load url
                          )

              UrlChanged url ->
                  ( model, Cmd.none )

              NoOpFrontendMsg ->
                  ( model, Cmd.none )


      updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
      updateFromBackend msg model =
          case msg of
              NoOpToFrontend ->
                  ( model, Cmd.none )


      view model =
          { title = ""
          , body =
              [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
                  [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
                  , Html.div
                      [ Attr.style "font-family" "sans-serif"
                      , Attr.style "padding-top" "40px"
                      ]
                      [ Html.text model.message ]
                  ]
              ]
          }
  |])
  , ("src/Backend.elm", [text|
      module Backend exposing (..)

      import Html
      import Lamdera exposing (ClientId, SessionId)
      import Types exposing (..)


      type alias Model =
          BackendModel


      app =
          Lamdera.backend
              { init = init
              , update = update
              , updateFromFrontend = updateFromFrontend
              , subscriptions = \m -> Sub.none
              }


      init : ( Model, Cmd BackendMsg )
      init =
          ( { message = "Hello!" }
          , Cmd.none
          )


      update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
      update msg model =
          case msg of
              NoOpBackendMsg ->
                  ( model, Cmd.none )


      updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
      updateFromFrontend sessionId clientId msg model =
          case msg of
              NoOpToBackend ->
                  ( model, Cmd.none )

  |])
  , ("src/Types.elm", [text|
      module Types exposing (..)

      import Browser exposing (UrlRequest)
      import Browser.Navigation exposing (Key)
      import Url exposing (Url)


      type alias FrontendModel =
          { key : Key
          , message : String
          }


      type alias BackendModel =
          { message : String
          }


      type FrontendMsg
          = UrlClicked UrlRequest
          | UrlChanged Url
          | NoOpFrontendMsg


      type ToBackend
          = NoOpToBackend


      type BackendMsg
          = NoOpBackendMsg


      type ToFrontend
          = NoOpToFrontend

  |])
  , ("src/Env.elm", emptyEnv)
  ]



-- @TODO are the following checks useful? Maybe it's better to rely on docs and the type
-- system, especially now that it's highly likely we'll be auto-generating the types for the user?


      -- Task.throw $ Lamdera.Exit.SomeProblem
      --   $ Help.report "ERROR" (Nothing)
      --     ("Looks like some files exist already, either finish implementing them, or delete them and re-run so I can help you generate defaults.")
      --     []

-- frontendAppDefined <- liftIO $ checkHasAppDefinition "src/Frontend.elm"
-- unless frontendAppDefined $
--   Task.throw $ Lamdera.Exit.SomeProblem
--     $ Help.report "MISSING IMPLEMENTATION" (Just "src/Frontend.elm")
--       ("")
--       [ D.reflow "I'm expecting to see an `app = Lamdera.frontend {...}` implementation here so I know where the app begins, similar to how Elm apps need a `main`."
--       [ D.reflow "See <https://dashboard.lamdera.app/docs/building> for a guide on how to implement these."
--       ]
--
--   throwError
--     "src/Frontend.elm is missing an `app` definition. \n\
--     \\n\
--     \Lamdera apps need an `app =` definition in Frontend.elm and Backend.elm so I know where the app begins, similar to how Elm apps need a `main`.\n\
--     \\n\
--     \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."
--
--
-- backendAppDefined <- liftIO $ checkHasAppDefinition "src/Backend.elm"
-- unless backendAppDefined $
--   throwError
--     "src/Backend.elm is missing an `app` definition. \n\
--     \\n\
--     \Lamdera apps need an `app =` definition in Frontend.elm and Backend.elm so I know where the app begins, similar to how Elm apps need a `main`.\n\
--     \\n\
--     \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."
--
--
-- msgHasTypes <- liftIO $ checkMsgHasTypes ["FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]
-- unless (msgHasTypes == True) $
--   throwError
--     "src/Types.elm is missing some type definitions.\n\
--     \\n\
--     \Lamdera apps need FrontendMsg, ToBackend, BackendMsg and ToFrontend types defined in Types.elm.\n\
--     \\n\
--     \Please see https://alpha.lamdera.app/development for how to set up a Lamdera app."


-- pure True

emptyEnv =
  [text|
    module Env exposing (..)

    -- The Env.elm file is for per-environment configuration.
    -- See https://dashboard.lamdera.app/docs/environment for more info.


    dummyConfigItem =
      ""
  |]
