module Log exposing (log)

{-| Debug.log in the dev build. The prod build (Debug is not allowed with
--optimize) uses an identity function instead, see ../log-prod/Log.elm
-}


log : String -> a -> a
log =
    Debug.log
