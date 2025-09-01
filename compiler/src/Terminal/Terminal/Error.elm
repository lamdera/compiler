{- MANUALLY FORMATTED -}
module Terminal.Terminal.Error exposing
  ( {-Error(..)
  , ArgError(..)
  , FlagError(..)
  , Expectation(..)
  , exitWithHelp
  , exitWithError
  , exitWithUnknown
  ,-} exitWithOverview
  )


import Compiler.Reporting.Doc as D exposing (d)
import Extra.System.IO as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Maybe as MMaybe
import Terminal.Command
import Terminal.Terminal.Internal as Internal



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Terminal.Command.GlobalState g h) v



-- EXIT


exitSuccess : TList D.Doc -> IO g h ()
exitSuccess =
  exitWith -- Exit.ExitSuccess


exitWith : TList D.Doc -> IO g h ()
exitWith docs =
  Terminal.Command.putDoc <|
    D.vcat <| MList.concatMap (\doc -> [doc,d""]) docs


getExeName : IO g h String
getExeName =
  -- hard coded
  IO.return "elm"


stack : TList D.Doc -> D.Doc
stack docs =
  D.vcat <| MList.intersperse (d"") docs



-- HELP


argsToDoc : String -> D.Doc
argsToDoc command =
  argsToDocHelp command


argsToDocHelp : String -> D.Doc
argsToDocHelp command =
  D.hang 4 <| D.hsep <| MList.map D.fromChars <|
    [ command ]



-- OVERVIEW


exitWithOverview : D.Doc -> D.Doc -> TList Internal.Command -> IO g h ()
exitWithOverview intro outro commands =
  IO.bind getExeName <| \exeName ->
    exitSuccess
      [ intro
      , d"The most common commands are:"
      , D.indent 4 <| stack <| MMaybe.mapMaybe (toSummary exeName) commands
      , d"There are a bunch of other commands as well though. Here is a full list:"
      , D.indent 4 <| D.dullcyan <| toCommandList exeName commands
      , d"Adding the --help flag gives a bunch of additional details about each one."
      , outro
      ]


toSummary : String -> Internal.Command -> Maybe D.Doc
toSummary exeName (Internal.Command name summary) =
  case summary of
    Internal.Uncommon ->
      Nothing

    Internal.Common summaryString ->
      Just <|
        D.vcat
          [ D.cyan <| argsToDoc (exeName ++ " " ++ name)
          , D.indent 4 <| D.reflow summaryString
          ]


toCommandList : String -> TList Internal.Command -> D.Doc
toCommandList exeName commands =
  let
    names = MList.map Internal.toName commands
    width = MList.maximum (MList.map String.length names)

    toExample name =
      D.fromChars <| exeName ++ " " ++ name ++ String.repeat (width - String.length name) " " ++ " --help"
  in
  D.vcat (MList.map toExample names)
