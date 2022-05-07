module Test.Wire_Tvar_Recursive exposing (..)

import Lamdera.Wire3


type alias FrontendModel =
    { currentPage : Page }


type Page
    = AdminPage_ AdminModel


type alias AdminModel =
    Replay FrontendModel


type alias Replay model =
    { model : model }



{- The addition of this function causes the type solver to no longer crash,
   even though it's not used. So this point to an Elm type solver issue.
   Probably the only way we can solve this is to start adding generated type
   signatures to the generated wire encoders.
-}
-- completelyIrrelevantAndUnused : AdminModel -> Lamdera.Wire3.Encoder
-- completelyIrrelevantAndUnused model =
--     w3_encode_AdminModel model
