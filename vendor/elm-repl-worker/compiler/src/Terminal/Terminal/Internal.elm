{- MANUALLY FORMATTED -}
module Terminal.Terminal.Internal exposing
  ( Command(..)
  , toName
  , Summary(..)
  --, Flags(..)
  --, Flag(..)
  --, Parser(..)
  --, Args(..)
  --, CompleteArgs(..)
  --, RequiredArgs(..)
  )



-- COMMAND


type Command =
  Command
    String
    Summary


toName : Command -> String
toName (Command name _) =
  name



{-| The information that shows when you run the executable with no arguments.
If you say it is `Common`, you need to tell people what it does. Try to keep
it to two or three lines. If you say it is `Uncommon` you can rely on `Details`
for a more complete explanation.
-}
type Summary = Common String | Uncommon
