module Extra.Type.Lens exposing
    ( Lens
    , modify
    )


type alias Lens whole part =
    { getter : whole -> part
    , setter : part -> whole -> whole
    }


modify : Lens a b -> (b -> b) -> a -> a
modify lens f a =
    lens.setter (f (lens.getter a)) a
