module Generate.Mode
  ( Mode(..)
  , isDebug
  , ShortFieldNames
  , shortenFieldNames
  -- @LAMDERA
  , legibleFieldNames
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Generate.JavaScript.Name as JsName


import Lamdera ((&))
import qualified Lamdera

-- MODE


data Mode
  = Dev (Maybe Extract.Types)
  | Prod ShortFieldNames


isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev mi -> Maybe.isJust mi
    Prod _ -> False



-- SHORTEN FIELD NAMES


type ShortFieldNames =
  Map.Map N.Name JsName.Name


shortenFieldNames :: Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets :: N.Name -> Int -> Map.Map Int [N.Name] -> Map.Map Int [N.Name]
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets


addToShortNames :: [N.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields


addField :: ShortFieldNames -> N.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames


-- @LAMDERA

legibleFieldNames :: Opt.GlobalGraph -> ShortFieldNames
legibleFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToNamesLegible Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies

addToNamesLegible :: [N.Name] -> ShortFieldNames -> ShortFieldNames
addToNamesLegible fields shortNames =
  -- Does not shorten field names, but adds them to the short names map
  List.foldl' (\shortNames field ->
    Map.insert field (JsName.fromLocal field) shortNames
  ) shortNames fields
