module Migrate_Nested_Dict.Old exposing (..)

import Dict exposing (Dict)


type Target
    = Wrapper (Dict Int (Dict Int Int))
