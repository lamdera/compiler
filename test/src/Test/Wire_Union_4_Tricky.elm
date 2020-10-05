module Test.Wire_Union_4_Tricky exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Lamdera.Wire2
import Set exposing (Set)
import Test.External exposing (..)


type UnionTricky
    = ValueStandalone
    | ValueCustomBasic ExternalCustomBasic
    | ValueRecursive UnionTricky



-- | ValueInt Int
-- | ValueFloat Float
-- | ValueBool Bool
-- | ValueChar Char
-- | ValueString String
-- | ValueOrder Order
-- | ValueUnit ()
-- | ValueTwoParams Bool Char
-- | ValueMaybe (Maybe String)
-- | ValueListBool (List Bool)
-- | ValueSetFloat (Set Float)
-- | ValueArrayString (Array String)
-- | ValueResult (Result String Int)
-- | ValueDict (Dict String (List Int))
-- | ValueTuple ( Int, String )
-- | ValueTriple ( Int, String, Bool )
-- Values above here are okay
--
--
--
--
--
-- | ValueTime Time.Posix
-- | ValueAliased AliasInt
--   -- | ValueSubRecursive (ExternalRecord Int) -- breaks wire
--   -- | ValueSubRecursive (ExternalRecord AllUnion) -- also breaks wire
-- | ValueSubRecursive SubRecursiveRecord -- also breaks wire
-- | ValueDeep Subdir.Subsubdir.SubsubdirType.DeepRec
-- | ValueCustom (ExternalCustom Int)
-- | ValueCustomDeep Subdir.Subsubdir.SubsubdirType.DeepCustom
-- | ValuePhantom (Phantom OnlyUsedInPhantom)
-- | ValueAliasTuple ExternalAliasTuple
-- | ValueAll AllTypes
