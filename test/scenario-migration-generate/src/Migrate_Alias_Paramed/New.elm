module Migrate_Alias_Paramed.New exposing (..)


type alias Box a =
    { item : Maybe a
    , count : Float
    }


type Target
    = Wrapper (Box Int)
