{- MANUALLY FORMATTED -}
module Terminal.Reactor exposing
  ( compile
  )


import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Extra.System.Dir exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Terminal.Command as Command



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Command.GlobalState g h) v



-- SERVE ELM


compile : FilePath -> IO g h (Either Exit.Reactor String)
compile path =
  IO.bind Stuff.findRoot <| \maybeRoot ->
  case maybeRoot of
    Nothing ->
      IO.return <| Left <| Exit.ReactorNoOutline

    Just root ->
      Task.run <|
        Task.bind (Task.eio Exit.ReactorBadDetails <| Details.load root) <| \details ->
        Task.bind (Task.eio Exit.ReactorBadBuild <| Build.fromPaths root details (NE.CList path [])) <| \artifacts ->
        Task.mapError Exit.ReactorBadGenerate <| Generate.dev root details artifacts
