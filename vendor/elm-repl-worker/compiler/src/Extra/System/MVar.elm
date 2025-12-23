module Extra.System.MVar exposing
    ( Lens
    , MVar
    , State
    , initialState
    , new
    , newEmpty
    , newWaiting
    , read
    , wait
    , write
    )

import Extra.System.IO as IO
import Extra.Type.Lens as Lens
import Extra.Type.Map as Map



-- TYPES


type alias Lens gs a =
    Lens.Lens gs (State gs a)


type MVar a
    = -- index into the state map
      MVar Int


type alias State gs v =
    -- next index, map of indexes to values
    ( Int, Map.Map Int (Entry gs v), String )


type Entry gs a
    = Waiting (() -> IO.IO gs a)
    | Done a


initialState : String -> State gs a
initialState name =
    ( 0, Map.empty, name )



-- INTERFACE


newEmpty : Lens gs a -> IO.IO gs (MVar a)
newEmpty { getter, setter } =
    IO.bind IO.get <|
        \gs ->
            let
                ( mvar, state ) =
                    newEmptyMVar (getter gs)
            in
            IO.put (setter state gs)
                |> IO.fmap (\() -> mvar)


newWaiting : Lens gs a -> (() -> IO.IO gs a) -> IO.IO gs (MVar a)
newWaiting lens f =
    IO.bind (newEmpty lens) <|
        \mvar ->
            wait lens mvar f
                |> IO.fmap (\() -> mvar)


new : Lens gs a -> a -> IO.IO gs (MVar a)
new lens a =
    IO.bind (newEmpty lens) <|
        \mvar ->
            write lens mvar a
                |> IO.fmap (\() -> mvar)


read : Lens gs a -> MVar a -> IO.IO gs a
read ({ getter } as lens) mvar =
    IO.bind IO.get <|
        \gs0 ->
            case getEntry mvar (getter gs0) of
                Waiting f ->
                    IO.bind (f ()) <|
                        \a ->
                            write lens mvar a
                                |> IO.fmap (\() -> a)

                Done a ->
                    IO.return a


wait : Lens gs a -> MVar a -> (() -> IO.IO gs a) -> IO.IO gs ()
wait lens mvar f =
    modify lens <| setEntry mvar (Waiting f)


write : Lens gs a -> MVar a -> a -> IO.IO gs ()
write lens mvar a =
    modify lens <| setEntry mvar (Done a)



-- PRIMITIVES


modify : Lens gs a -> (State gs a -> State gs a) -> IO.IO gs ()
modify { getter, setter } f =
    IO.modify (\gs -> setter (f (getter gs)) gs)


getEntry : MVar a -> State gs a -> Entry gs a
getEntry (MVar id) ( _, map, _ ) =
    Map.ex map id


setEntry : MVar a -> Entry gs a -> State gs a -> State gs a
setEntry (MVar id) entry ( nextId, map, name ) =
    ( nextId, Map.insert id entry map, name )


newEmptyMVar : State gs a -> ( MVar a, State gs a )
newEmptyMVar ( nextId, map, name ) =
    ( MVar nextId, ( nextId + 1, map, name ) )
