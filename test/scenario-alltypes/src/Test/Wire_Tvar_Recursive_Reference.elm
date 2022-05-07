module Test.Wire_Tvar_Recursive_Reference exposing (..)

import Lamdera.Wire exposing (..)
import Test.Wire_Tvar_Recursive


{-| These issues might be related:

<https://github.com/elm/compiler/issues/2119>
<https://github.com/elm/compiler/issues/2207>

Adding a type signature here doesn't change this error
`reference : Test.Wire_Tvar_Recursive.FrontendModel -> Encoder`

But uncommenting `completelyIrrelevantAndUnused` in `Test.Wire_Tvar_Recursive` does

-}
reference =
    Test.Wire_Tvar_Recursive.w3_encode_FrontendModel
