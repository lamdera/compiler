module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import WebGL.Texture as Texture


type alias FrontendModel =
    { key : Key
    , texture : Maybe Texture.Texture
    }


type alias BackendModel =
    { message : String
    , texture : Maybe Texture.Texture -- This should also cause an error
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | TextureLoaded (Result Texture.Error Texture.Texture)
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
