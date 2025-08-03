module TestBadWebGL exposing (..)

-- This should fail compilation because WebGL.Texture is in ToBackend
import WebGL.Texture as Texture


type ToBackend
    = SendTexture Texture.Texture  -- This should cause an error


type alias BackendModel =
    { texture : Maybe Texture.Texture  -- This should also cause an error
    }