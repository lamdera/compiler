module Migrate_External_Paramed.New exposing (..)

import Evergreen.V2.External
import Evergreen.V2.Types


type Target
    = UserTvarAlias (Evergreen.V2.External.Paramed CustomType)



-- | UserTvarAlias2 (Evergreen.V2.External.Paramed2 CustomType Evergreen.V2.External.AllCoreTypes)
-- | UserTvarAliasSub (Evergreen.V2.External.ParamedSub Evergreen.V2.IncludedByParam.Custom)


type CustomType
    = CustomOne
    | CustomTwo
