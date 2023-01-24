module Migrate_External_Paramed.New exposing (..)

import Chart.Item as CI
import Evergreen.V2.External
import Evergreen.V2.Types


type Target
    = UserTvarAlias (Evergreen.V2.External.Paramed CustomType)
    | UserMixPackage AnalyticsModel


type alias AnalyticsModel =
    { hoveringBars : List (CI.One Datum CI.Bar)
    , hoveringDots : List (CI.One Datum CI.Dot)
    }


type alias Datum =
    ( Int, Maybe AnalyticsVisits )


type alias AnalyticsVisits =
    { start : Int
    , count : Int
    }



-- | UserTvarAlias2 (Evergreen.V2.External.Paramed2 CustomType Evergreen.V2.External.AllCoreTypes)
-- | UserTvarAliasSub (Evergreen.V2.External.ParamedSub Evergreen.V2.IncludedByParam.Custom)


type CustomType
    = CustomOne
    | CustomTwo
