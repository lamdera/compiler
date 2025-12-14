{- MANUALLY FORMATTED -}
module Terminal.Terminal exposing
  ( app
  , Command, command
  , Summary, common, uncommon
  )


import Compiler.Reporting.Doc as D
import Extra.System.IO as IO
import Extra.Type.List exposing (TList)
import Terminal.Command
import Terminal.Terminal.Error as Error
import Terminal.Terminal.Internal as Internal



-- FROM INTERNAL


type alias Command = Internal.Command
command = Internal.Command

type alias Summary = Internal.Summary
common = Internal.Common
uncommon = Internal.Uncommon



-- PRIVATE IO


type alias IO g h v =
  IO.IO (Terminal.Command.GlobalState g h) v



-- APP


app : D.Doc -> D.Doc -> TList Command -> IO g h ()
app intro outro commands =
  Error.exitWithOverview intro outro commands
