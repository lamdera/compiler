{- MANUALLY FORMATTED -}
module Builder.Reporting.Task exposing
  ( Task, fmap, pure, liftA2, return, bind, andThen
  , run
  , throw
  , mapError
  --
  , io
  , mio
  , eio
  )


import Extra.Class.Functor as Functor
import Extra.Class.Applicative as Applicative
import Extra.Class.Monad as Monad
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))



-- TASKS


type Task z s x a =
  Task ((a -> IO s z) -> (x -> IO s z) -> IO s z)


run : Task (Either x a) s x a -> IO s (Either x a)
run (Task task) =
  task (IO.return << Right) (IO.return << Left)


throw : x -> Task z s x a
throw x =
  Task <| \_ err -> err x


mapError : (x -> y) -> Task z s x a -> Task z s y a
mapError func (Task task) =
  Task <| \ok err ->
    task ok (err << func)



-- IO


io : IO s a -> Task z s x a
io work =
  Task <| \ok _ -> IO.bind work ok


mio : x -> IO s (Maybe a) -> Task z s x a
mio x work =
  Task <| \ok err ->
    IO.bind work <| \result ->
    case result of
      Just a -> ok a
      Nothing -> err x


eio : (x -> y) -> IO s (Either x a) -> Task z s y a
eio func work =
  Task <| \ok err ->
    IO.bind work <| \result ->
    case result of
      Right a -> ok a
      Left x -> err (func x)



-- INSTANCES


fmap : Functor.Fmap a (Task z s x a) b (Task z s x b)
fmap func (Task taskA) =
  Task <| \ok err ->
    let
      okA arg = ok (func arg)
    in
    taskA okA err


pure : Applicative.Pure a (Task z s x a)
pure a =
  Task <| \ok _ -> ok a

andMap : Applicative.AndMap (Task z s x a) (Task z s x (a -> b)) (Task z s x b)
andMap (Task taskArg) (Task taskFunc) =
  Task <| \ok err ->
    let
      okFunc func =
        let
          okArg arg = ok (func arg)
        in
        taskArg okArg err
    in
    taskFunc okFunc err

liftA2 : Applicative.LiftA2 a (Task z s x a) b (Task z s x b) c (Task z s x c)
liftA2 = Applicative.liftA2 fmap andMap


return : Monad.Return a (Task z s x a)
return = pure

bind : Monad.Bind a (Task z s x a) (Task z s x b)
bind (Task taskA) callback =
  Task <| \ok err ->
    let
      okA a =
        case callback a of
          Task taskB -> taskB ok err
    in
    taskA okA err

andThen : Monad.AndThen a (Task z s x a) (Task z s x b)
andThen = Monad.andThen bind
