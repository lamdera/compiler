module Types exposing (..)

{-| Lamdera app's Types module (as opposed to the SelectList's Types module -
see elm.json in this test)
-}


type alias FrontendModel =
    { message : String
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
