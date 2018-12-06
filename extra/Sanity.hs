module Sanity where

import qualified Data.Map as Map
import Interpreter.PrettyPrint

(!) m k =
  case Map.lookup k m of
    Just v -> v
    Nothing -> error (sShow (Map.keys m, "couldn't find key", k, "in map"))
