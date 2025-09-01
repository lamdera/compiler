module Extra.System.IO exposing
    ( Cont
    , IO
    , ION(..)
    , andThen
    , bind
    , bindSequence
    , fmap
    , get
    , getLens
    , init
    , join
    , liftA2
    , liftCmd
    , liftCmdIO
    , liftCont
    , log
    , modify
    , modifyLens
    , noOp
    , now
    , pure
    , put
    , putLens
    , return
    , rmap
    , sequence
    , sleep
    , update
    , when
    )

-- impure IO implemented as the State monad with an explicit state type and impure TEA commands

import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.Lens as Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Process
import Task
import Time



-- TYPES


type alias IO s a =
    -- FreeT (Sum Cmd (ContT () (IO s))) (State s) a
    s -> ( ION s a, s )


type ION s a
    = Pure a
    | ImpureCmd (Cmd (IO s a))
    | ImpureCont (Cont s (IO s a))



-- MISC


bindSequence : TList (IO s ()) -> IO s a -> IO s a
bindSequence ms ma =
    bind (sequence ms) (\_ -> ma)


sequence : TList (IO s ()) -> IO s ()
sequence ms =
    MList.foldr (\m acc -> bind m (\() -> acc)) noOp ms


sleep : Float -> IO s ()
sleep time =
    liftTask (Process.sleep time)


now : IO s Time.Posix
now =
    liftTask Time.now


log : String -> a -> IO s ()
log msg a s =
    ( (\_ -> Pure ()) (Debug.log msg a), s )


noOp : IO s ()
noOp =
    return ()


init : (flags -> s) -> (flags -> IO s ()) -> flags -> ( s, Cmd (IO s ()) )
init initialModel initialIO flags =
    update (initialIO flags) (initialModel flags)


update : IO s () -> s -> ( s, Cmd (IO s ()) )
update msg model =
    case msg model of
        ( Pure (), newModel ) ->
            ( newModel, Cmd.none )

        ( ImpureCmd cmd, newModel ) ->
            ( newModel, cmd )

        ( ImpureCont cont, newModel ) ->
            update (cont identity) newModel



-- LIFT


liftCmd : Cmd a -> IO s a
liftCmd cmd =
    liftCmdIO (Cmd.map return cmd)


liftCmdIO : Cmd (IO s a) -> IO s a
liftCmdIO cmd s =
    ( ImpureCmd cmd, s )


liftCont : Cont s a -> IO s a
liftCont cont =
    liftContIO (contFmap return cont)


liftContIO : Cont s (IO s a) -> IO s a
liftContIO cont s =
    ( ImpureCont cont, s )


liftTask : Task.Task Never a -> IO s a
liftTask task =
    liftCmdIO (Task.perform return task)



-- STATE OPERATIONS


get : IO s s
get s =
    ( Pure s, s )


getLens : Lens s2 s1 -> IO s2 s1
getLens lens =
    rmap get lens.getter


modify : (s -> s) -> IO s ()
modify f s =
    ( Pure (), f s )


modifyLens : Lens s2 s1 -> (s1 -> s1) -> IO s2 ()
modifyLens lens f =
    modify (Lens.modify lens f)


put : s -> IO s ()
put s _ =
    ( Pure (), s )


putLens : Lens s2 s1 -> s1 -> IO s2 ()
putLens lens s1 =
    modify (lens.setter s1)



-- TYPE CLASS INSTANCES


andThen : Monad.AndThen a (IO s a) (IO s b)
andThen f ma =
    bind ma f


bind : Monad.Bind a (IO s a) (IO s b)
bind ma f s0 =
    case ma s0 of
        ( Pure a, s1 ) ->
            f a s1

        ( ImpureCmd cmd, s1 ) ->
            ( ImpureCmd (Cmd.map (\ima -> bind ima f) cmd), s1 )

        ( ImpureCont cont, s1 ) ->
            ( ImpureCont (contFmap (\ima -> bind ima f) cont), s1 )


fmap : Functor.Fmap a (IO s a) b (IO s b)
fmap f ma =
    bind ma (\a -> return (f a))


join : IO s (IO s a) -> IO s a
join mma =
    bind mma identity


liftA2 : Applicative.LiftA2 a (IO s a) b (IO s b) c (IO s c)
liftA2 f ma mb =
    bind ma (\a -> bind mb (\b -> return (f a b)))


pure : Applicative.Pure a (IO s a)
pure =
    return


return : Monad.Return a (IO s a)
return a s =
    ( Pure a, s )


rmap : IO s a -> (a -> b) -> IO s b
rmap ma f =
    fmap f ma


when : Applicative.When (IO s ())
when c f =
    Applicative.when return c f



--


type alias Cont s a =
    -- ContT () (IO s) a
    (a -> IO s ()) -> IO s ()


contFmap : Functor.Fmap a (Cont s a) b (Cont s b)
contFmap f ca ret =
    ca (\a -> ret (f a))
