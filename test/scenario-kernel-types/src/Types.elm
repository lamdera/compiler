module Types exposing (..)

{-| Every JS kernel-backed type a Lamdera type can hold, and where lamdera check lets it
in (Lamdera.TypeHash, Lamdera.Wire3.Helpers.KernelCodec):

  - linear-algebra's Vec2/Vec3/Vec4/Mat4 (Float64Arrays) have real codecs: anywhere
  - WebGL Mesh is plain Elm data (made of those): anywhere
  - Browser.Navigation.Key is a data-free password in Lamdera's elm/browser: anywhere
  - File, WebGL Texture, Shader and Entity are JS refs: FrontendModel and FrontendMsg only
  - Regex, Html, Http bodies and the rest: nowhere

-}

import Browser.Navigation
import File exposing (File)
import Html exposing (Html)
import Http
import Json.Encode
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Regex exposing (Regex)
import WebGL
import WebGL.Texture


type alias Vecs =
    { v2 : Vec2
    , v3 : Vec3
    , v4 : Vec4
    , m4 : Mat4
    }


type alias Data =
    { vecs : Vecs
    , mesh : WebGL.Mesh { position : Vec3 }
    , key : Browser.Navigation.Key
    }


type alias Refs =
    { file : File
    , texture : WebGL.Texture.Texture
    , shader : WebGL.Shader { position : Vec3 } {} {}
    , entity : WebGL.Entity
    }


type alias FrontendModel =
    { data : Data
    , refs : Refs
    }


type FrontendMsg
    = FrontendData Data
    | FrontendRefs Refs


type alias BackendModel =
    { data : Data
    }


type BackendMsg
    = BackendData Data


type ToBackend
    = ToBackendData Data
    | ToBackendShader (WebGL.Shader { position : Vec3 } {} {})
    | ToBackendEntity WebGL.Entity


type ToFrontend
    = ToFrontendData Data


type alias Banned =
    { regex : Regex
    , html : Html ()
    , body : Http.Body
    , value : Json.Encode.Value
    }
