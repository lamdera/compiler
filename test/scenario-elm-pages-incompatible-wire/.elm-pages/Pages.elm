module Pages exposing (builtAt)

import Time
import Json.Decode
import Json.Encode


builtAt : Time.Posix
builtAt =
    Time.millisToPosix 1732476538115
