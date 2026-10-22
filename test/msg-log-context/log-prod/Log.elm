module Log exposing (log)

{-| The prod build cannot use Debug.log, see ../log-dev/Log.elm
-}


log : String -> a -> a
log _ a =
    a
