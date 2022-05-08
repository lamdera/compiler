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



{- The addition of this function used to cause the type solver to crash,
   even though it's not used. So this pointed to an Elm type solver issue.

   We solved this by implementing generated type signatures to the generated
   wire encoders, but leaving this here for future refence to the original
   note living in Test.Wire_Tvar_Recursive_Reference.
-}
-- completelyIrrelevantAndUnused : AdminModel -> Lamdera.Wire3.Encoder
-- completelyIrrelevantAndUnused model =
--     w3_encode_AdminModel model
