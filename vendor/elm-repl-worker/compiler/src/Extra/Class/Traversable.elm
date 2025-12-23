module Extra.Class.Traversable exposing
    ( SequenceA
    , Traverse
    , sequenceA
    )


type alias Traverse a ta fb ftb =
    (a -> fb) -> ta -> ftb


type alias SequenceA tfa fta =
    tfa -> fta


sequenceA :
    Traverse fa tfa fa fta
    -> SequenceA tfa fta
sequenceA pTraverse =
    pTraverse identity
