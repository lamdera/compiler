module Global exposing (State(..))


type State a b c d e f g h
    = State
        -- Config
        a
        -- Dir
        b
        -- Details
        c
        -- Build
        d
        -- Generate
        e
        -- Terminal
        f
        -- Repl
        g
        -- App
        h
