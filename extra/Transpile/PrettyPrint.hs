module Transpile.PrettyPrint where

import qualified Data.Text.Lazy as T
import Data.Function ((&))

sShow a = pformat 0 $! show a -- T.unpack $! pShow a
tShow a = T.pack $ sShow a

pformat ident x =
  let
    ind = repeat ' ' & take (ident*2)
    indl = repeat ' ' & take ((ident-1)*2)
  in
  case x of
    '\n':rest -> "\n" ++ ind ++ pformat ident rest
    ',':rest -> "\n" ++ indl ++ "," ++ pformat ident rest
    '(':')':rest -> "()" ++ pformat ident rest
    '[':']':rest -> "[]" ++ pformat ident rest
    '(':rest -> "\n" ++ ind ++ "(" ++ pformat (ident+1) rest
    '[':rest -> "\n" ++ ind ++ "[" ++ pformat (ident+1) rest
    ')':rest -> ")\n" ++ indl ++ pformat (ident-1) rest
    ']':rest -> "]\n" ++ indl ++ pformat (ident-1) rest
    x:rest -> x : pformat ident rest
    [] -> ""


