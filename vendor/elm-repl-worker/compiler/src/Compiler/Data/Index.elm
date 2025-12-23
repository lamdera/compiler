module Compiler.Data.Index exposing
    ( VerifiedList(..)
    , ZeroBased(..)
    , bZeroBased
    , first
    , indexedMap
    , indexedTraverse
    , indexedZipWith
    , indexedZipWithA
    , next
    , second
    , third
    , toHuman
    , toMachine
    )

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)



-- ZERO BASED


type ZeroBased
    = ZeroBased Int


bZeroBased : B.Binary ZeroBased
bZeroBased =
    B.bin1 ZeroBased (\(ZeroBased n) -> n) B.bWord64


first : ZeroBased
first =
    ZeroBased 0


second : ZeroBased
second =
    ZeroBased 1


third : ZeroBased
third =
    ZeroBased 2


next : ZeroBased -> ZeroBased
next (ZeroBased i) =
    ZeroBased (i + 1)



-- DESTRUCT


toMachine : ZeroBased -> Int
toMachine (ZeroBased index) =
    index


toHuman : ZeroBased -> Int
toHuman (ZeroBased index) =
    index + 1



-- INDEXED MAP


indexedMap : (ZeroBased -> a -> b) -> TList a -> TList b
indexedMap func xs =
    MList.zipWith func (MList.map ZeroBased (MList.range 0 (MList.length xs))) xs


indexedTraverse :
    Applicative.Pure (TList b) flb
    -> Applicative.LiftA2 b fb (TList b) flb (TList b) flb
    -> ((ZeroBased -> a -> fb) -> TList a -> flb)
indexedTraverse pPure pLiftA2 func xs =
    MList.sequenceA pPure pLiftA2 (indexedMap func xs)



-- VERIFIED/INDEXED ZIP


type VerifiedList a
    = LengthMatch (TList a)
    | LengthMismatch Int Int


indexedZipWith : (ZeroBased -> a -> b -> c) -> TList a -> TList b -> VerifiedList c
indexedZipWith func listX listY =
    indexedZipWithHelp func 0 listX listY []


indexedZipWithHelp : (ZeroBased -> a -> b -> c) -> Int -> TList a -> TList b -> TList c -> VerifiedList c
indexedZipWithHelp func index listX listY revListZ =
    case ( listX, listY ) of
        ( [], [] ) ->
            LengthMatch (MList.reverse revListZ)

        ( x :: xs, y :: ys ) ->
            indexedZipWithHelp func (index + 1) xs ys <|
                func (ZeroBased index) x y
                    :: revListZ

        _ ->
            LengthMismatch (index + MList.length listX) (index + MList.length listY)


indexedZipWithA :
    Applicative.Pure (TList c) flc
    -> Applicative.Pure (VerifiedList c) fvlc
    -> Functor.Fmap (TList c) flc (VerifiedList c) fvlc
    -> Applicative.LiftA2 c fc (TList c) flc (TList c) flc
    -> ((ZeroBased -> a -> b -> fc) -> TList a -> TList b -> fvlc)
indexedZipWithA pPure pPure2 pMap pLiftA2 func listX listY =
    case indexedZipWith func listX listY of
        LengthMatch xs ->
            pMap LengthMatch <| MList.sequenceA pPure pLiftA2 xs

        LengthMismatch x y ->
            pPure2 (LengthMismatch x y)
