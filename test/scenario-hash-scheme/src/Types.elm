module Types exposing (..)

{-| The types whose hashing is easiest to change by accident, each where lamdera check
allows it, so all six core types hash. Test.TypeHashes "hash scheme" pins their hashes
per Lamdera.Version.hashScheme. Add a type here when hashing gains a special case.
-}

import Browser.Navigation
import File exposing (File)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
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


type ToFrontend
    = ToFrontendData Data
