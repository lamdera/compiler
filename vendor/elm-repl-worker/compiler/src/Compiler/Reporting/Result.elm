{- MANUALLY FORMATTED -}
module Compiler.Reporting.Result exposing
  ( TResult(..), RStep(..)
  , run
  , ok
  , warn
  , throw
  --
  , fmap
  , pure, andMap, liftA2, discardFirst
  , return, bind, andThen
  , Step(..), loop
  , foldM, traverseList, sequenceAMap, traverseMap, traverseWithKey
  , mergeA, mapMissing, zipWithAMatched
  )


import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Warning as Warning
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type TResult info warnings error a =
  CResult (info -> warnings -> RStep info warnings error a)


type RStep info warnings error a
  = Rbad info warnings (OneOrMore.OneOrMore error)
  | Rgood info warnings a


run : TResult () (TList w) e a -> (TList w, Either (OneOrMore.OneOrMore e) a)
run (CResult k) =
  case k () [] of
    Rbad () w e -> (MList.reverse w, Left e)
    Rgood () w a -> (MList.reverse w, Right a)



-- HELPERS


ok : a -> TResult i w e a
ok a =
  CResult <| \i w ->
    Rgood i w a


warn : Warning.Warning -> TResult i (TList Warning.Warning) e ()
warn warning =
  CResult <| \i warnings ->
    Rgood i (warning::warnings) ()


throw : e -> TResult i w e a
throw e =
  CResult <| \i w ->
    Rbad i w (OneOrMore.one e)



-- FANCY INSTANCE STUFF


fmap : Functor.Fmap a (TResult i w e a) b (TResult i w e b)
fmap func (CResult k) =
  CResult <| \i w ->
    case k i w of
      Rbad i1 w1 e ->
        Rbad i1 w1 e
      Rgood i1 w1 value ->
        Rgood i1 w1 (func value)


pure : Applicative.Pure a (TResult i w e a)
pure = ok


andMap : Applicative.AndMap (TResult i w e a) (TResult i w e (a -> b)) (TResult i w e b)
andMap (CResult kv) (CResult kf) =
  CResult <| \i w ->
    case kf i w of
      Rbad i1 w1 e1 ->
        case kv i1 w1 of
          Rbad i2 w2 e2 -> Rbad i2 w2 (OneOrMore.more e1 e2)
          Rgood i2 w2 _ -> Rbad i2 w2 e1

      Rgood i1 w1 func ->
        case kv i1 w1 of
          Rbad i2 w2 e2 -> Rbad i2 w2 e2
          Rgood i2 w2 value -> Rgood i2 w2 (func value)


liftA2 : Applicative.LiftA2 a (TResult i w e a) b (TResult i w e b) c (TResult i w e c)
liftA2 =
  Applicative.liftA2 fmap andMap


discardFirst : Applicative.DiscardFirst (TResult i w e a) (TResult i w e b)
discardFirst =
  Applicative.discardFirst liftA2


return : Monad.Return a (TResult i w e a)
return = ok


bind : Monad.Bind a (TResult i w e a) (TResult i w e b)
bind (CResult ka) callback =
  CResult <| \i w ->
    case ka i w of
      Rbad i1 w1 e ->
        Rbad i1 w1 e
      Rgood i1 w1 a ->
        case callback a of
          CResult kb -> kb i1 w1


andThen : Monad.AndThen a (TResult i w e a) (TResult i w e b)
andThen =
  Monad.andThen bind

-- PERF add INLINE to these?


-- LOOP


type Step state a
  = Loop state
  | Done a


loop : (state -> TResult i w e (Step state a)) -> state -> TResult i w e a
loop callback state =
  CResult <| \i w ->
    loopHelp callback i w state


loopHelp : (state -> TResult i w e (Step state a)) -> i -> w -> state -> RStep i w e a
loopHelp callback i w state =
  case callback state of
    CResult k ->
      case k i w of
        Rbad i1 w1 e -> Rbad i1 w1 e
        Rgood i1 w1 (Loop newState) -> loopHelp callback i1 w1 newState
        Rgood i1 w1 (Done a) -> Rgood i1 w1 a



-- FOLD AND TRAVERSAL


foldM : (b -> a -> TResult i w e b) -> b -> TList a -> TResult i w e b
foldM callback zero list =
  loop (foldMHelp callback) (list, zero)


foldMHelp : (b -> a -> TResult i w e b) -> (TList a, b) -> TResult i w e (Step (TList a, b) b)
foldMHelp callback (list, result) =
  case list of
    [] ->
      return (Done result)
    a::rest ->
      fmap (\b -> Loop (rest, b)) (callback result a)


traverseList : (a -> TResult i w e b) -> TList a -> TResult i w e (TList b)
traverseList callback list =
  loop (traverseListHelp callback) (list, [])


traverseListHelp : (a -> TResult i w e b) -> (TList a, TList b) -> TResult i w e (Step (TList a, TList b) (TList b))
traverseListHelp callback (list, result) =
  case list of
    [] ->
      return (Done (MList.reverse result))
    a::rest ->
      fmap (\b -> Loop (rest, b::result)) (callback a)


sequenceAMap : Map.Map comparable (TResult i w e a) -> TResult i w e (Map.Map comparable a)
sequenceAMap map =
  loop sequenceAMapHelp (Map.toList map, Map.empty)


sequenceAMapHelp : (TList (comparable, TResult i w e a), Map.Map comparable a) -> TResult i w e (Step (TList (comparable, TResult i w e a), Map.Map comparable a) (Map.Map comparable a))
sequenceAMapHelp (pairs, result) =
  case pairs of
    [] ->
      return (Done result)
    (k, r)::rest ->
      fmap (\a -> Loop (rest, Map.insert k a result)) r


traverseMap : (a -> TResult i w e b) -> Map.Map comparable a -> TResult i w e (Map.Map comparable b)
traverseMap callback map =
  loop (traverseMapHelp callback) (Map.toList map, Map.empty)


traverseMapHelp : (a -> TResult i w e b) -> (TList (comparable, a), Map.Map comparable b) -> TResult i w e (Step (TList (comparable, a), Map.Map comparable b) (Map.Map comparable b))
traverseMapHelp callback (pairs, result) =
  case pairs of
    [] ->
      return (Done result)
    (k, a)::rest ->
      fmap (\b -> Loop (rest, Map.insert k b result)) (callback a)


traverseWithKey : (comparable -> a -> TResult i w e b) -> Map.Map comparable a -> TResult i w e (Map.Map comparable b)
traverseWithKey callback map =
  loop (traverseWithKeyHelp callback) (Map.toList map, Map.empty)


traverseWithKeyHelp : (comparable -> a -> TResult i w e b) -> (TList (comparable, a), Map.Map comparable b) -> TResult i w e (Step (TList (comparable, a), Map.Map comparable b) (Map.Map comparable b))
traverseWithKeyHelp callback (pairs, result) =
  case pairs of
    [] ->
      return (Done result)
    (k, a)::rest ->
      fmap (\b -> Loop (rest, Map.insert k b result)) (callback k a)



-- MERGE


mergeA
  : (comparable -> a -> TResult i w e (Maybe c))
  -> (comparable -> b -> TResult i w e (Maybe c))
  -> (comparable -> a -> b -> TResult i w e (Maybe c))
  -> Map.Map comparable a
  -> Map.Map comparable b
  -> TResult i w e (Map.Map comparable c)
mergeA missA missB zipAB ma mb =
  bind (loop (mergeAHelp1 missA missB zipAB) (Map.toList mb, (Map.toList ma, Map.empty))) <| \ (leftovers, intermediateResult) ->
  loop (mergeAHelp2 missA) (leftovers, intermediateResult)


mergeAHelp1
  : (comparable -> a -> TResult i w e (Maybe c))
  -> (comparable -> b -> TResult i w e (Maybe c))
  -> (comparable -> a -> b -> TResult i w e (Maybe c))
  -> (TList (comparable, b), (TList (comparable, a), Map.Map comparable c ))
  -> TResult i w e (Step (TList (comparable, b), (TList (comparable, a), Map.Map comparable c)) (TList (comparable, a), Map.Map comparable c))
mergeAHelp1 missA missB zipAB (pairsB, ((pairsA, map) as state)) =
  case pairsB of
    [] ->
      return (Done state)

    (kB,b)::restB ->
      case pairsA of
        [] ->
          fmap (\mc -> Loop (loop1 kB mc restB pairsA map)) (missB kB b)
        (kA,a)::restA ->
          if kA < kB then
            fmap (\mc -> Loop (loop1 kA mc pairsB restA map)) (missA kA a)
          else if kA > kB then
            fmap (\mc -> Loop (loop1 kB mc restB pairsA map)) (missB kB b)
          else
            fmap (\mc -> Loop (loop1 kA mc restB restA map)) (zipAB kA a b)


mergeAHelp2
  : (comparable -> a -> TResult i w e (Maybe c))
  -> (TList (comparable, a), Map.Map comparable c)
  -> TResult i w e (Step (TList (comparable, a), Map.Map comparable c) (Map.Map comparable c))
mergeAHelp2 missA (pairsA, map) =
  case pairsA of
    [] ->
      return (Done map)

    (k,a)::rest ->
      fmap (\mc -> Loop (loop2 k mc rest map)) (missA k a)


loop1
  : comparable -> Maybe c
  -> TList (comparable, b) -> TList (comparable, a) -> Map.Map comparable c
  -> (TList (comparable, b), (TList (comparable, a), Map.Map comparable c))
loop1 k mc pairsB pairsA map =
  ( pairsB, loop2 k mc pairsA map )


loop2
  : comparable -> Maybe c
  -> TList (comparable, a) -> Map.Map comparable c
  -> (TList (comparable, a), Map.Map comparable c)
loop2 k mc pairsA map =
  ( pairsA
  , ( case mc of
        Just c -> Map.insert k c map
        Nothing -> map
    )
  )

mapMissing : (comparable -> a -> b) -> comparable -> a -> TResult i w e (Maybe b)
mapMissing f k a =
  pure (Just (f k a))


zipWithAMatched : (comparable -> a -> b -> TResult i w e c) -> comparable -> a -> b -> TResult i w e (Maybe c)
zipWithAMatched f k a b =
  fmap Just (f k a b)
