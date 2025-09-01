module Extra.Type.Tuple exposing (mappend, traverseSecond)

import Extra.Class.Functor as Functor
import Extra.Class.Monoid as Monoid
import Extra.Class.Traversable as Traversable


mappend : Monoid.Mappend a -> Monoid.Mappend b -> Monoid.Mappend ( a, b )
mappend pMappendA pMappendB ( a1, b1 ) ( a2, b2 ) =
    ( pMappendA a1 a2, pMappendB b1 b2 )


traverseSecond :
    Functor.Fmap b fb ( x, b ) fxb
    -> Traversable.Traverse a ( x, a ) fb fxb
traverseSecond pFmap f ( x, a ) =
    pFmap (\b -> ( x, b )) (f a)
