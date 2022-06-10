module Evergreen.V2.Types exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId)
import Time exposing (Posix)
import Url exposing (Url)


type alias BackendModel =
    { basic : Float
    }


type alias FrontendModel =
    { basic : Int
    }


type FrontendMsg
    = WindowResized Int Int
    | UrlClicked UrlRequest
    | UrlChanged Url
    | OpenedPage Page
    | FormAppNameChanged String
    | FormSshKeyChanged String
    | FormEmailChanged String
    | FormPasswordChanged String
    | AgreeTermsChanged Bool
    | FormAppCreateSubmitted
    | FormLoginSubmitted
      -- Admin: User creation
    | NewUserCreated
    | Noop


type BackendMsg
    = NoOpBackendMsg
    | AppCreatedResponse ClientId AppId (Result Http.Error String) -- We sure we want to use Http.Error...?


type ToBackend
    = AppCreated { sshKey : String, appId : String, email : String }
    | ApplicationListRequested Username
    | UserLoginSubmitted { username : Username, hash : PasswordHash }


type ToFrontend
    = AppCreationConfirmed AppInfo
    | AppCreationError Http.Error
    | ApplicationListResponse (Result String (List AppInfo))
    | UserLoginResponse (Result String { username : Username })


type Page
    = Login
    | Dashboard
    | CreateApp


type alias Session =
    { username : Username

    -- , token : String @TODO add this in future with rotating token
    }


type alias AppInfo =
    { appId : AppId
    , username : Username
    }


type alias AppId =
    String


type alias User =
    { created : Posix
    , lastActive : Posix
    , username : Username
    , name : Maybe String
    , passwordHash : PasswordHash
    , role : List UserRole
    }


type alias Username =
    String


type
    PasswordHash
    -- | PasswordHash2 Sha1HmacKeyBase64 Sha1HmacDigestBase64 -- Our planned second password scheme once hmac package is upgraded
    = PasswordHash1 Sha1DigestBase64 -- Our first password scheme


type alias Sha1DigestBase64 =
    String


type UserRole
    = AlphaUser
    | Admin
