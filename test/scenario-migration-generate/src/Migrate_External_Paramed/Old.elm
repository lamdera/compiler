module Migrate_External_Paramed.Old exposing (..)

import Evergreen.V1.External
import Evergreen.V1.Types


type Target
    = UserTvarAlias (Evergreen.V1.External.Paramed CustomType)



-- | UserTvarAlias2 (Evergreen.V1.External.Paramed2 CustomType Evergreen.V1.External.AllCoreTypes)
-- | UserTvarAliasSub (Evergreen.V1.External.ParamedSub Evergreen.V1.IncludedByParam.Custom)


type CustomType
    = CustomOne
    | CustomTwo
