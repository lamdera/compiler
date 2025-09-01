{- MANUALLY FORMATTED -}
module Compiler.Reporting.Render.Type.Localizer exposing
  ( Localizer
  , toDoc
  , toChars
  , empty
  --, fromNames
  , fromModule
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Extra.Type.List as MList
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set



-- LOCALIZER


type Localizer =
  Localizer (Map.Map Name.Name Import)


type Import =
  Import
    {- alias -} (Maybe Name.Name)
    {- exposing -} Exposing


type Exposing
  = All
  | Only (Set.Set Name.Name)


empty : Localizer
empty =
  Localizer Map.empty



-- LOCALIZE


toDoc : Localizer -> ModuleName.Canonical -> Name.Name -> D.Doc
toDoc localizer home name =
  D.fromChars (toChars localizer home name)


toChars : Localizer -> ModuleName.Canonical -> Name.Name -> String
toChars (Localizer localizer) ((ModuleName.Canonical _ home) as moduleName) name =
  case Map.lookup home localizer of
    Nothing ->
      home ++ "." ++ name

    Just (Import alias exposing_) ->
      case exposing_ of
        All ->
          name

        Only set ->
          if Set.member name set then
            name
          else if name == Name.list && moduleName == ModuleName.list then
            "List"
          else
            MMaybe.maybe home identity alias ++ "." ++ name



-- FROM MODULE


fromModule : Src.Module -> Localizer
fromModule ((Src.Module _ _ imports _ _ _ _ _) as modul) =
  Localizer <| Map.fromList <|
    (Src.getName modul, Import Nothing All) :: MList.map toPair imports


toPair : Src.Import -> (Name.Name, Import)
toPair (Src.Import (A.At _ name) alias exposing_) =
  ( name
  , Import alias (toExposing exposing_)
  )


toExposing : Src.Exposing -> Exposing
toExposing exposing_ =
  case exposing_ of
    Src.Open ->
      All

    Src.Explicit exposedList ->
      Only (MList.foldr addType Set.empty exposedList)


addType : Src.Exposed -> Set.Set Name.Name -> Set.Set Name.Name
addType exposed types =
  case exposed of
    Src.Lower _               -> types
    Src.Upper (A.At _ name) _ -> Set.insert name types
    Src.Operator _ _          -> types
