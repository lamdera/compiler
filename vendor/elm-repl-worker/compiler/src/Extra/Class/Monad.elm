module Extra.Class.Monad exposing
    ( AndThen
    , Bind
    , Return
    , andThen
    )

import Extra.Class.Applicative as Applicative


type alias Return a ma =
    Applicative.Pure a ma


type alias Bind a ma mb =
    ma -> (a -> mb) -> mb


type alias AndThen a ma mb =
    -- <<=
    (a -> mb) -> ma -> mb


andThen :
    Bind a ma mb
    -> AndThen a ma mb
andThen pBind f ma =
    pBind ma f
