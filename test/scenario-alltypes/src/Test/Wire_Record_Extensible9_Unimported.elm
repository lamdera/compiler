module Test.Wire_Record_Extensible9_Unimported exposing (..)

{-| Regression: an extensible record alias applied in a module that does not import the
modules its fields' types come from.

    -- Model, which imports Remote
    type alias Model m = { m | pipelines : WebData (List Int), remote : Remote Bool String }

    -- here, which imports Model but not Remote
    type alias Session = Model { name : String }

The alias chain is inlined here (an extensible record has to be), so the codecs for
`WebData` and `Remote` are referenced from this module, whose imports do not include
their module: their signatures could not be looked up, and the failure signatures used
instead take no codec arguments. With type arguments to pass, that was TOO MANY ARGS.
Found in concourse's `web/elm` (`Application.Models.Session` over `SideBar.Model`, whose
`pipelines : WebData (List Concourse.Pipeline)` comes from krisajenkins/remotedata).

-}

import Test.Wire_Record_Extensible9_Unimported_Model as Model


type alias Session =
    Model.Model { name : String }


type alias SessionOf m =
    Model.Model { m | name : String }
