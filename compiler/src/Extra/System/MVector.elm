module Extra.System.MVector exposing
    ( IO
    , MVector
    , State
    , grow
    , init
    , length
    , modify
    , read
    , replicate
    , unsafeFreeze
    , write
    )

import Extra.System.IO.Pure as IO
import Extra.System.IORef as IORef exposing (IORef)
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map exposing (Map)


type MVector a
    = -- id in the state map and length
      MVector Int Int


type alias IO s a =
    IO.IO (State s) a


type alias State a =
    -- next id, map of ids to vectors, IORef state
    ( Int, Map Int (Map Int (IORef a)), IORef.State a )


init : State a
init =
    ( 0, Map.empty, IORef.init )


grow : MVector a -> Int -> IO a (MVector a)
grow ((MVector id len) as mv) n (( nextId, map, refState ) as state) =
    case Map.lookup id map of
        Just vec ->
            ( MVector nextId (len + n)
            , let
                ( copy, nextRefState ) =
                    IO.traverseMap (\r -> IO.bind (IORef.read r) IORef.new) vec refState
              in
              ( nextId + 1
              , Map.insert nextId copy map
              , nextRefState
              )
            )

        Nothing ->
            ( mv, state )


length : MVector a -> Int
length (MVector _ len) =
    len


modify : MVector a -> (a -> a) -> Int -> IO a ()
modify (MVector id _) f i (( nextId, map, refState ) as state) =
    case getRef id i map of
        Just ref ->
            liftResult nextId map <| IORef.modify ref f refState

        Nothing ->
            ( (), state )


read : MVector a -> Int -> IO a a
read (MVector id _) i ( nextId, map, refState ) =
    case getRef id i map of
        Just ref ->
            liftResult nextId map <| IORef.read ref refState

        Nothing ->
            Debug.todo "MVector.read: id not found"


replicate : Int -> a -> IO a (MVector a)
replicate n a ( nextId, map, refState ) =
    let
        go : Int -> IORef.IO a (Map Int (IORef a)) -> IORef.IO a (Map Int (IORef a))
        go i vecIO =
            if i < n then
                go (i + 1) (IO.liftA2 (Map.insert i) (IORef.new a) vecIO)

            else
                vecIO

        ( vec, nextRefState ) =
            go 0 (IO.pure Map.empty) refState
    in
    ( MVector nextId n
    , ( nextId + 1
      , Map.insert nextId vec map
      , nextRefState
      )
    )


unsafeFreeze : MVector a -> IO a (TList a)
unsafeFreeze (MVector id _) (( nextId, map, refState ) as state) =
    case Map.lookup id map of
        Just vec ->
            liftResult nextId map <| IO.traverseList IORef.read (Map.elems vec) refState

        Nothing ->
            ( [], state )


write : MVector a -> Int -> a -> IO a ()
write (MVector id _) i a (( nextId, map, refState ) as state) =
    case getRef id i map of
        Just ref ->
            liftResult nextId map <| IORef.write ref a refState

        Nothing ->
            ( (), state )


getRef : Int -> Int -> Map Int (Map Int (IORef a)) -> Maybe (IORef a)
getRef id i map =
    Map.lookup id map |> Maybe.andThen (\vec -> Map.lookup i vec)


liftResult : Int -> Map Int (Map Int (IORef s)) -> ( a, IORef.State s ) -> ( a, State s )
liftResult nextId map ( a, refState ) =
    ( a, ( nextId, map, refState ) )
