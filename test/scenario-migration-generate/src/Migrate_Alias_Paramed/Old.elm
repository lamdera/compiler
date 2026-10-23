module Migrate_Alias_Paramed.Old exposing (..)


type alias Box a =
    { item : Maybe a
    , count : Int
    }


type Target
    = Wrapper (Box Int)
