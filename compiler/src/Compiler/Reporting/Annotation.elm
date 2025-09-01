module Compiler.Reporting.Annotation exposing
    ( Located(..)
    , Position(..)
    , Region(..)
    , at
    , bRegion
    , merge
    , mergeRegions
    , one
    , toRegion
    , toValue
    , traverse
    , zero
    )

import Extra.Class.Functor as Functor
import Extra.Class.Traversable as Traversable
import Extra.Data.Binary as B



-- LOCATED


type Located a
    = At Region a


traverse :
    Functor.Fmap b fb (Located b) flb
    -> Traversable.Traverse a (Located a) fb flb
traverse pFmap func (At region value) =
    pFmap (At region) <| func value


toValue : Located a -> a
toValue (At _ value) =
    value


merge : Located a -> Located b -> value -> Located value
merge (At r1 _) (At r2 _) value =
    At (mergeRegions r1 r2) value



-- POSITION


type Position
    = Position Int Int


at : Position -> Position -> a -> Located a
at start end a =
    At (Region start end) a



-- REGION


type Region
    = Region Position Position


toRegion : Located a -> Region
toRegion (At region _) =
    region


mergeRegions : Region -> Region -> Region
mergeRegions (Region start _) (Region _ end) =
    Region start end


zero : Region
zero =
    Region (Position 0 0) (Position 0 0)


one : Region
one =
    Region (Position 1 1) (Position 1 1)


bRegion : B.Binary Region
bRegion =
    B.bin2 Region (\(Region a b) -> B.T2 a b) bPosition bPosition


bPosition : B.Binary Position
bPosition =
    B.bin2 Position (\(Position a b) -> B.T2 a b) B.bWord16 B.bWord16
