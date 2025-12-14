{- MANUALLY FORMATTED -}
module Compiler.Generate.Mode exposing
  ( Mode(..)
  , isDebug
  , ShortFieldNames
  , shortenFieldNames
  )


import Compiler.AST.Optimized as Opt
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type.Extract as Extract
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- MODE


type Mode
  = Dev (Maybe Extract.Types)
  | Prod ShortFieldNames


isDebug : Mode -> Bool
isDebug mode =
  case mode of
    Dev mi -> MMaybe.isJust mi
    Prod _ -> False



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
