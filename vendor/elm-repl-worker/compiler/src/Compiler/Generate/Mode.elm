{- MANUALLY FORMATTED -}
module Compiler.Generate.Mode exposing
  ( Mode(..)
  , isDebug
  , ShortFieldNames
  , shortenFieldNames
  --
  , DevMode(..)
  , isAsync
  , isAsyncActive
  , deActivate
  )


import Compiler.AST.Optimized as Opt
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type.Extract as Extract
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- MODE


type Mode
  = Dev DevMode
  | Prod ShortFieldNames


{- NEW: DevMode -}
type DevMode
  = DevNormal
  | DevDebug Extract.Types
  | DevAsync Bool (Set.Set Name.Name)


isDebug : Mode -> Bool
isDebug mode =
  case mode of
    Dev (DevDebug _) -> True
    _ -> False


{- NEW: isAsyncActive -}
isAsyncActive : Mode -> Bool
isAsyncActive mode =
  case mode of
    Dev (DevAsync True _) -> True
    _ -> False


{- NEW: isAsync -}
isAsync : Mode -> Bool
isAsync mode =
  case mode of
    Dev (DevAsync _ _) -> True
    _ -> False


deActivate : Mode -> Mode
deActivate mode =
  case mode of
    Dev (DevAsync True suspendFuns) -> Dev (DevAsync False suspendFuns)
    _ -> mode



-- SHORTEN FIELD NAMES


type alias ShortFieldNames =
  Map.Map Name.Name JsName.Name


shortenFieldNames : Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty <|
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets : Name.Name -> Int -> Map.Map Int (TList Name.Name) -> Map.Map Int (TList Name.Name)
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets


addToShortNames : TList Name.Name -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  MList.foldl addField shortNames fields


addField : ShortFieldNames -> Name.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames
