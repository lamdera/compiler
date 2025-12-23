module Extra.Class.Functor exposing (Fmap)


type alias Fmap a fa b fb =
    -- <$>
    (a -> b) -> fa -> fb
