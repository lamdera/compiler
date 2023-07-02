module Sanity where

{-

Helpers for dealing with `Map.!: given key is not an element in the map` errors.

To be used with ./addSanity.sh and ./removeSanity.sh bash scripts.

See extra/readme.md for more info.

-}

import qualified Data.Map as Map
import Wire.PrettyPrint
import GHC.Stack (HasCallStack)


{- An alternative version of (!) that will print where it was called from -}
(!) :: (Ord c, HasCallStack) => Map.Map c a -> c -> a
(!) m k =
    case Map.lookup k m of
      Just v -> v
      Nothing ->
        error "Sanity: (!) failed!"


{- An alternative version of (Prelude.last) that will print where it was called from -}
last :: String -> [a] -> a
last identifier list =
  case list of
    [] -> error $ "last called on empty list in " <> identifier
    _ -> Prelude.last list


{- An alternative version of (!) that will print the keys and missing key on failure -}
debugFind :: (Ord c, Show c, HasCallStack) => Map.Map c a -> c -> a
debugFind m k =
  case Map.lookup k m of
    Just v -> v
    Nothing ->
      error (sShow ("Sanity: (!) failed", take 10 (Map.keys m), "does not contain:", k))


{- An alternative version of (!) that will print the keys, missing key & some added context -}
debugFindCtx :: (Ord c, Show c, Show ctx, HasCallStack) => Map.Map c a -> c -> ctx -> a
debugFindCtx m k ctx =
  case Map.lookup k m of
    Just v -> v
    Nothing ->
      error (sShow ("Sanity: (!) failed", Map.keys m, "does not contain:", k, "with context:", ctx))
