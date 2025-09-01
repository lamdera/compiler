{- MANUALLY FORMATTED -}
module Builder.Http exposing
  ( Manager
  , getManager
  , toUrl
  ---- fetch
  , get
  , post
  --, Header
  --, accept
  , Error(..)
  ---- archives
  --, Sha
  --, shaToChars
  , getArchive
  ---- upload
  --, upload
  --, filePart
  --, jsonPart
  --, stringPart
  )


import Compiler.Elm.Version as V
import Extra.System.Config as Config
import Extra.System.Exception exposing (handle)
import Extra.System.Http as Sys
import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List exposing (TList)
import Zip



-- PRIVATE IO


type alias IO b c d e f g h v =
  IO.IO (Config.GlobalState b c d e f g h) v



-- MANAGER


type alias Manager =
  Sys.Manager


getManager : IO b c d e f g h Manager
getManager =
  Sys.defaultManager



-- URL


toUrl : String -> TList (String,String) -> String
toUrl url params =
  case params of
    []  -> url
    _::_ -> url ++ "?" ++ Sys.urlEncodeVars params



-- FETCH


get : Sys.Manager -> String -> TList Sys.Header -> (Error -> x) -> (String -> IO b c d e f g h (Either x v)) -> IO b c d e f g h (Either x v)
get =
  fetch Sys.methodGet


post : Sys.Manager -> String -> TList Sys.Header -> (Error -> x) -> (String -> IO b c d e f g h (Either x v)) -> IO b c d e f g h (Either x v)
post =
  fetch Sys.methodPost


fetch : Sys.Method -> Sys.Manager -> String -> TList Sys.Header -> (Error -> x) -> (String -> IO b c d e f g h (Either x v)) -> IO b c d e f g h (Either x v)
fetch methodVerb manager url headers onError onSuccess =
  handle (handleHttpException url onError) <|
  IO.bind (Sys.parseUrlThrow url) <| \req0 ->
  let req1 =
        { req0
          | method = methodVerb
          , headers = addDefaultHeaders headers
          } in
  Sys.withStringResponse req1 manager <| \response ->
    case response of
      Left err ->
        IO.return <| Left err
      Right string ->
        IO.fmap Right (onSuccess string)


addDefaultHeaders : TList Sys.Header -> TList Sys.Header
addDefaultHeaders headers =
  Sys.userAgent userAgent :: headers


userAgent : String
userAgent =
  "elm/" ++ V.toChars V.compiler



-- EXCEPTIONS


type Error
  = BadHttp String Sys.Exception


handleHttpException : String -> (Error -> x) -> Sys.Exception -> IO b c d e f g h (Either x v)
handleHttpException url onError httpException =
  IO.return (Left (onError (BadHttp url httpException)))



-- FETCH ARCHIVE


getArchive :
  Manager
  -> String
  -> (Error -> x)
  -> x
  -> (Zip.Zip -> IO b c d e f g h (Either x v))
  -> IO b c d e f g h (Either x v)
getArchive manager url onError err onSuccess =
  handle (handleHttpException url onError) <|
  IO.bind (Sys.parseUrlThrow url) <| \req0 ->
  let req1 =
        { req0
          | method = Sys.methodGet
          , headers = addDefaultHeaders []
          } in
  Sys.withBytesResponse req1 manager <| \response ->
    case response of
      Left error ->
        IO.return <| Left error

      Right bytes ->
        case Zip.fromBytes bytes of
          Nothing ->
            IO.return (Right (Left err))

          Just zip ->
            IO.fmap Right (onSuccess zip)
