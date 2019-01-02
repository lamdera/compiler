module SimpleTwo exposing (..)

-- import Json.Decode.Exploration exposing (decodeString)

nameCollisionTest = 42

recordCase a =
  case a of
    ({y,z}, 3) -> y+2
    (x, k) -> k+3

recordLambda =
  \{a,b,c} {d,e,f} -> b + f

normalLambda =
  \a b -> a


-- decStr = decodeString
