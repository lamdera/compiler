module Migrate_Nested_Dict.New exposing (..)

import Dict exposing (Dict)


type Target
    = Wrapper (Dict String (Dict String Int))
