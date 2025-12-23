module Extra.Class.Applicative exposing
    ( AndMap
    , DiscardFirst
    , LiftA2
    , Pure
    , When
    , andMap
    , discardFirst
    , liftA2
    , when
    )

import Extra.Class.Functor as Functor


type alias Pure a fa =
    a -> fa


type alias AndMap fa fab fb =
    -- flipped <*>
    fa -> fab -> fb


andMap :
    LiftA2 (a -> b) fab a fa b fb
    -> AndMap fa fab fb
andMap pLiftA2 fa fab =
    pLiftA2 identity fab fa


type alias LiftA2 a fa b fb c fc =
    (a -> b -> c) -> fa -> fb -> fc


liftA2 :
    Functor.Fmap a fa (b -> c) fbc
    -> AndMap fb fbc fc
    -> LiftA2 a fa b fb c fc
liftA2 pFmap pAndMap func fa fb =
    pAndMap fb (pFmap func fa)


type alias DiscardFirst fa fb =
    -- *>
    fa -> fb -> fb


discardFirst :
    LiftA2 a fa b fb b fb
    -> DiscardFirst fa fb
discardFirst pLiftA2 fa fb =
    pLiftA2 (\_ b -> b) fa fb


type alias When fu =
    Bool -> (() -> fu) -> fu


when :
    Pure () fu
    -> When fu
when mPure c f =
    if c then
        f ()

    else
        mPure ()
