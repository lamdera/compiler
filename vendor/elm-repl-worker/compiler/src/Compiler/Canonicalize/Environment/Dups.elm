{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Environment.Dups exposing
  ( detect
  , checkFields
  , checkFields_
  , TDict
  , none
  , one
  , insert
  , union
  , unions
  )


import Compiler.Data.Name as Name
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- DUPLICATE TRACKER


type alias TDict value =
  Map.Map Name.Name (OneOrMore.OneOrMore (Info value))


type Info value =
  Info
    {- region -} A.Region
    {- value -} value



-- DETECT


type alias ToError =
  Name.Name -> A.Region -> A.Region -> Error.Error


detect : ToError -> TDict a -> MResult.TResult i w Error.Error (Map.Map Name.Name a)
detect toError dict =
  MResult.traverseWithKey (detectHelp toError) dict


detectHelp : ToError -> Name.Name -> OneOrMore.OneOrMore (Info a) -> MResult.TResult i w Error.Error a
detectHelp toError name values =
  case values of
    OneOrMore.One (Info _ value) ->
      MResult.return value

    OneOrMore.More left right ->
      let
        (Info r1 _, Info r2 _) =
          OneOrMore.getFirstTwo left right
      in
      MResult.throw (toError name r1 r2)



-- CHECK FIELDS


checkFields : TList (A.Located Name.Name, a) -> MResult.TResult i w Error.Error (Map.Map Name.Name a)
checkFields fields =
  detect Error.DuplicateField (MList.foldr addField none fields)


addField : (A.Located Name.Name, a) -> TDict a -> TDict a
addField (A.At region name, value) dups =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region value)) dups


checkFields_ : (A.Region -> a -> b) -> TList (A.Located Name.Name, a) -> MResult.TResult i w Error.Error (Map.Map Name.Name b)
checkFields_ toValue fields =
  detect Error.DuplicateField (MList.foldr (addField_ toValue) none fields)


addField_ : (A.Region -> a -> b) -> (A.Located Name.Name, a) -> TDict b -> TDict b
addField_ toValue (A.At region name, value) dups =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region (toValue region value))) dups



-- BUILDING DICTIONARIES


none : TDict a
none =
  Map.empty


one : Name.Name -> A.Region -> value -> TDict value
one name region value =
  Map.singleton name (OneOrMore.one (Info region value))


insert : Name.Name -> A.Region -> a -> TDict a -> TDict a
insert name region value dict =
  Map.insertWith (\new old -> OneOrMore.more old new) name (OneOrMore.one (Info region value)) dict


union : TDict a -> TDict a -> TDict a
union a b =
  Map.unionWith OneOrMore.more a b


unions : TList (TDict a) -> TDict a
unions dicts =
  Map.unionsWith MList.foldl OneOrMore.more dicts
