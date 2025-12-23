module Extra.System.IORef exposing
    ( IO
    , IORef
    , State
    , init
    , modify
    , new
    , read
    , write
    )

import Extra.System.IO.Pure as IO
import Extra.Type.Map as Map exposing (Map)


type IORef a
    = -- index into the state map
      IORef Int


type alias IO s a =
    IO.IO (State s) a


type alias State a =
    -- next index, map of indexes to values
    ( Int, Map Int a )


init : State a
init =
    ( 0, Map.empty )


new : a -> IO a (IORef a)
new a ( nextId, map ) =
    ( IORef nextId, ( nextId + 1, Map.insert nextId a map ) )


read : IORef a -> IO a a
read (IORef id) (( _, map ) as state) =
    case Map.lookup id map of
        Just a ->
            ( a, state )

        Nothing ->
            Debug.todo "IORef.read: id not found"


write : IORef a -> a -> IO a ()
write (IORef id) a ( nextId, map ) =
    ( (), ( nextId, Map.insert id a map ) )


modify : IORef a -> (a -> a) -> IO a ()
modify ref f state =
    let
        ( a, state_ ) =
            read ref state
    in
    write ref (f a) state_
