module Sanity where

import qualified Data.Map as Map
import Wire.PrettyPrint

(!) :: (Ord c, Show c, Show ctx) => (Map.Map c a, ctx) -> c -> a
(!) (m, ctx) k =
  case Map.lookup k m of
    Just v -> v
    Nothing -> error (sShow ("Sanity:", Map.keys m, "couldn't find key", k, "in map context", ctx))
