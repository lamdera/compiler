{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Init where

import System.FilePath ((</>))
import qualified System.Directory as Dir
import NeatInterpolation

import Lamdera
import qualified Ext.Common


writeDefaultImplementations :: IO ()
writeDefaultImplementations = do
  root <- getProjectRoot "writeDefaultImplementations"
  defaultImplementations
    & mapM (\(filename, implementation) -> do
        exists <- doesFileExist (root </> filename)
        if exists
          then atomicPutStrLn $ "  Skipping " <> (root </> filename) <> ", already exists"
          else writeUtf8 (root </> filename) implementation
      )
  writeLineIfMissing "elm-stuff" (root </> ".gitignore")
  onlyWhen_ (fmap not $ Dir.doesDirectoryExist (root </> ".git")) $ do
    Ext.Common.cq_ "git" ["init"] ""
    pure ()


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
                          , Nav.pushUrl model.key (Url.toString url)
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


      view : Model -> Browser.Document FrontendMsg
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


emptyEnv :: Text
emptyEnv =
  [text|
    module Env exposing (..)

    -- The Env.elm file is for per-environment configuration.
    -- See https://dashboard.lamdera.app/docs/environment for more info.


    dummyConfigItem =
      ""
  |]