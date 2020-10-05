module Sanity where

import qualified Data.Map as Map
import Wire.PrettyPrint
import GHC.Stack (HasCallStack)


(!) :: (Ord c, HasCallStack) => Map.Map c a -> c -> a
(!) m k =
    case Map.lookup k m of
      Just v -> v
      Nothing ->
        error "Sanity failed!"



debug :: (Ord c, Show c, Show ctx, HasCallStack) => Map.Map c a -> c -> ctx -> a
debug m k ctx =
  case Map.lookup k m of
    Just v -> v
    Nothing ->
      error (sShow ("Sanity:", Map.keys m, "couldn't find key", k, "with context", ctx))



-- foo :: HasCallStack => String -> String
-- foo s = let ((name, _):_) = getCallStack callStack
--          in s <> ": " <> name


-- (!) :: (Ord c, Show c, Show ctx) => (Map.Map c a, ctx) -> c -> a
-- (!) (m, ctx) k =
--   case Map.lookup k m of
--     Just v -> v
--     Nothing -> error (sShow ("Sanity:", Map.keys m, "couldn't find key", k, "in map context", ctx))
--
--
--
--
--
--
