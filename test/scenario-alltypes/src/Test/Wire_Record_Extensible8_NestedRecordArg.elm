module Test.Wire_Record_Extensible8_NestedRecordArg exposing (..)

{-| Regression: extensible record aliases nested two deep around an extensible record,
where the aliases name their parameter the same as the referencing alias does.

    type alias NestedTooltip m = { m | hovered : Int }
    type alias NestedFavorites m = { m | favorites : List Int }
    type alias NestedModel m = NestedTooltip (NestedFavorites { m | expanded : Bool })

Resolving NestedTooltip's row tail `m` (bound to `NestedFavorites { m | expanded : Bool }`)
resolved that argument's own `m` with NestedTooltip's parameters again, which is the very
binding being expanded, so codegen never terminated: `lamdera make` grew to 22-47GB and
was killed. Found in concourse's `web/elm` (`SideBar.SideBar.Model`, over `Tooltip.Model`
and `Favorites.Model`), which official Elm builds in 0.25s.

`NestedModelRenamed` is the same shape with distinct parameter names, which always
terminated. Both now generate the same codecs.

-}

import Lamdera.Wire3


type alias NestedTooltip m =
    { m | hovered : Int }


type alias NestedFavorites m =
    { m | favorites : List Int }


type alias NestedModel m =
    NestedTooltip (NestedFavorites { m | expanded : Bool })


type alias NestedTooltipRenamed a =
    { a | hovered : Int }


type alias NestedFavoritesRenamed b =
    { b | favorites : List Int }


type alias NestedModelRenamed m =
    NestedTooltipRenamed (NestedFavoritesRenamed { m | expanded : Bool })


{-| A concrete record through the same chain, as an app's model would be -}
type alias NestedConcrete =
    NestedModel { name : String }
