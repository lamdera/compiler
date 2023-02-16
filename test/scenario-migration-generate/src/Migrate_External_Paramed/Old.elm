module Migrate_External_Paramed.Old exposing (..)

import Chart.Item as CI
import Dict exposing (Dict)
import Evergreen.V1.External
import Evergreen.V1.Types
import Quantity


type Target
    = UserTvarAlias (Evergreen.V1.External.Paramed CustomType)
    | UserMixPackage AnalyticsModel
    | UserMixPackage2 (Coord CustomType)


type alias AnalyticsModel =
    { hoveringBars : List (CI.One Datum CI.Bar)
    , hoveringDots : List (CI.One Datum CI.Dot)
    , previousCursorPositions :
        IdDict
            String
            { position : Point2d String String
            }
    }


type alias Point2d units coordinates =
    Point2d_ units coordinates


type Point2d_ units coordinates
    = Point2d_
        { x : Float
        , y : Float
        }


type NColor
    = Red
    | Black


type IdDict k v
    = RBNode_elm_builtin NColor Int v (IdDict k v) (IdDict k v)
    | RBEmpty_elm_builtin


type alias Datum =
    ( Int, Maybe AnalyticsVisits )


type alias AnalyticsVisits =
    { start : Int
    , count : Int
    }



-- | UserTvarAlias2 (Evergreen.V1.External.Paramed2 CustomType Evergreen.V1.External.AllCoreTypes)
-- | UserTvarAliasSub (Evergreen.V1.External.ParamedSub Evergreen.V1.IncludedByParam.Custom)


type CustomType
    = CustomOne
    | CustomTwo


type alias Coord units =
    ( Quantity.Quantity Int units, Quantity.Quantity Int units )
