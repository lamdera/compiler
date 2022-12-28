{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen.MigrationGeneratorHelpers where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N
import Data.Map.Strict (unionWithKey)

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface

import Lamdera
import Lamdera.Types
import StandaloneInstances


type TypeRef = (ModuleName.Canonical, N.Name)
type RecursionSet = Set.Set (ModuleName.Canonical, N.Name)
type TypeIdentifier = (Pkg.Author, Pkg.Project, N.Name, N.Name)

-- A specialised local migration def, along with any dependant top-level migrations and their related imports
data Migration = MigrationNested
  { migrationDef :: Text
  , migrationImports :: Set.Set ModuleName.Canonical
  , migrationTopLevelDefs :: MigrationDefinitions
  }
  deriving (Show)

xMigrationNested (a,b,c) = MigrationNested a b c

type MigrationDefinitions = Map TypeRef MigrationDefinition

-- Set of top-level migration functions and their required imports
data MigrationDefinition =
  MigrationDefinition
    { imports :: Set.Set ModuleName.Canonical
    , migrations :: Text
    }
  deriving (Show, Eq)


-- Map <import name> <package>
-- @TODO in future we can use this to pin package versions and adjust import routing to those snapshots
type ElmImports = Set.Set ModuleName.Canonical


allMigrations :: MigrationDefinitions -> Text
allMigrations efts =
  efts
    & Map.toList
    & fmap (\(file, ef@(MigrationDefinition imports migrations)) -> migrations )
    & List.sort
    & T.intercalate "\n\n"

importsToText :: Set.Set ModuleName.Canonical -> [Text]
importsToText imports =
  imports
    & Set.toList
    & fmap (\(ModuleName.Canonical (Pkg.Name author project) module_) ->
      "import " <> nameToText module_
    )
    & List.sort

allSubDefs :: [Migration] -> MigrationDefinitions
allSubDefs migrations =
  migrations & foldl (\acc migration ->
    acc & Map.union (migrationTopLevelDefs migration)
  ) Map.empty

allImports :: [Migration] -> ElmImports
allImports migrations = migrations
  & fmap (\migration ->
    migrationImports migration <> (migrationTopLevelDefs migration & Map.toList & fmap (imports . snd) & mergeAllImports)
  )
  & mergeAllImports

mergeAllSubDefs :: [MigrationDefinitions] -> MigrationDefinitions
mergeAllSubDefs ftss = ftss & foldl (\acc fts -> Map.union acc fts) Map.empty

mergeAllImports :: [ElmImports] -> ElmImports
mergeAllImports imps = imps & foldl (\acc elmImport -> acc <> elmImport) Set.empty

migrationNameUnderscored :: N.Name -> Int -> Int -> N.Name -> Text
migrationNameUnderscored newModule oldVersion newVersion newTypeName =
  newModule
    & N.toText
    & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ""
    & T.replace ("Evergreen.V" <> show_ oldVersion <> ".") ""
    & T.replace "." "_"
    & (\v -> "migrate_" <> v <> "_" <> nameToText newTypeName)


data TypeDef = Alias Can.Alias | Union Can.Union deriving (Show)


unimplemented :: Text -> Text -> Migration
unimplemented debugIdentifier message =
  let debugIdentifier_ :: Text = ""
        -- & (\v -> debugIdentifier & suffixIfNonempty " ")
  in
  xMigrationNested (T.concat ["Unimplemented -- ", debugIdentifier_, message, "\n"], Set.empty, Map.empty)


canModuleName :: ModuleName.Canonical -> N.Name
canModuleName (ModuleName.Canonical (Pkg.Name author pkg) module_) = module_

moduleKey :: TypeIdentifier -> Text
moduleKey identifier@(author, pkg, module_, tipe) =
  if utf8ToText author == "author" then
    -- Internal package, keep as is
    nameToText module_
  else
    -- External package
    utf8ToText author <> "/" <> utf8ToText pkg <> ":" <> nameToText module_

idTypeName :: TypeIdentifier -> Text
idTypeName identifier@(author, pkg, module_, tipe) = N.toText tipe

moduleNameKey :: ModuleName.Canonical -> Text
moduleNameKey moduleName =
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
      if author == "author" then
        -- Internal package, keep as is
        nameToText module_
      else
        -- External package
        utf8ToText author <> "/" <> utf8ToText pkg <> ":" <> nameToText module_


getModuleNameUnkeyed :: ModuleName.Canonical -> Text
getModuleNameUnkeyed moduleName =
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
        nameToText module_

typeNameToStringQualified :: ModuleName.Canonical -> N.Name -> [Can.Type] -> Text
typeNameToStringQualified moduleName tipeName params = do
  let coreType = [nameToText tipeName] ++ parenthesize (fmap qualifiedTypeName params) & T.intercalate " "
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
      case (author, pkg, module_) of
        ("elm", "core", "Basics") -> coreType
        ("elm", "core", "String") -> coreType
        ("elm", "core", "Maybe") -> coreType
        ("elm", "core", "List") -> coreType
        ("elm", "core", "Set") -> coreType
        ("elm", "core", "Array") -> coreType
        ("elm", "core", "Dict") -> coreType
        ("elm", "core", "Result") -> coreType
        _ ->
          T.concat $ [nameToText module_, ".", nameToText tipeName] ++ (fmap qualifiedTypeName params)


parenthesize :: [Text] -> [Text]
parenthesize texts =
  texts & fmap (\v -> T.concat ["(", v, ")"])


asIdentifier :: Can.Type -> TypeIdentifier
asIdentifier tipe =
  case tipe of
    Can.TType moduleName name params -> asIdentifier_ (moduleName, name)
    Can.TAlias moduleName name _ _ ->   asIdentifier_ (moduleName, name)
    Can.TLambda a b        -> ("elm", "core", "Basics", "<function>")
    Can.TVar a             -> ("elm", "core", "Basics", "a")
    Can.TRecord a b        -> ("elm", "core", "Basics", "{}")
    Can.TUnit              -> ("elm", "core", "Basics", "()")
    Can.TTuple a b c       -> ("elm", "core", "Basics", "Tuple")


asIdentifier_ :: (ModuleName.Canonical, N.Name) -> TypeIdentifier
asIdentifier_ pair =
  case pair of
    ((ModuleName.Canonical (Pkg.Name author pkg) module_), typeName) ->
          (author, pkg, module_, typeName)


asTypeName :: Can.Type -> Text
asTypeName tipe =
  case tipe of
    Can.TType moduleName name params -> N.toText name
    Can.TAlias moduleName name _ _ -> N.toText name
    Can.TRecord _ _ -> "anonymousRecord_"
    _ -> error $ "unimplemented asTypeName: " <> show tipe


qualifiedTypeName :: Can.Type -> Text
qualifiedTypeName tipe =
  case tipe of
    Can.TType moduleName name params ->         typeNameToStringQualified moduleName name params
    Can.TAlias moduleName name namedParams _ -> typeNameToStringQualified moduleName name (fmap snd namedParams)
    Can.TLambda a b        -> "<function>"
    Can.TVar a             -> "a"
    Can.TRecord a b        -> "{}"
    Can.TUnit              -> "()"
    Can.TTuple a b mc      ->
      case mc of
        Just c -> T.concat ["(", qualifiedTypeName a, ", ", qualifiedTypeName b, ", ", qualifiedTypeName c, ")"]
        Nothing -> T.concat ["(", qualifiedTypeName a, ", ", qualifiedTypeName b, ")"]


isUserType :: TypeIdentifier -> Bool
isUserType (author, pkg, module_, tipe) =
  author == "author" && pkg == "project"

-- Whether this top level wrapping type is defined by the user,
-- i.e. an alias or custom type, meaning we likely need to migrate it
isUserDefinedType_ :: Can.Type -> Bool
isUserDefinedType_ cType =
  -- (\v ->
  --   if v
  --     then v
  --     else debugHaskellPass "isUserDefinedType_" cType v
  -- ) $
  case cType of
    Can.TType moduleName name params ->
      isUserModule moduleName

    Can.TLambda _ _ -> True
    Can.TVar _ -> True
    Can.TRecord _ _ -> True
    Can.TUnit -> False
    Can.TTuple _ _ _ -> False
    Can.TAlias _ _ _ aType ->
      case aType of
        Can.Holey t -> isUserDefinedType_ t
        Can.Filled t -> isUserDefinedType_ t


containsUserTypes :: [(N.Name, Can.Type)] -> Can.Type -> Bool
containsUserTypes tvarMap tipe =
  case tipe of
    Can.TType moduleName name params -> isUserModule moduleName || (tvarResolveParams params tvarMap & any (containsUserTypes tvarMap))
    Can.TAlias moduleName name namedParams aType ->
      case aType of
        Can.Holey t -> containsUserTypes tvarMap t || (namedParams & fmap snd & (\params -> tvarResolveParams params tvarMap) & any (containsUserTypes tvarMap))
        Can.Filled t -> containsUserTypes tvarMap t || (namedParams & fmap snd & (\params -> tvarResolveParams params tvarMap) & any (containsUserTypes tvarMap))

    Can.TLambda a b        -> False -- not true but unsupported type
    Can.TVar a             ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (_,ti) -> containsUserTypes tvarMap ti
        Nothing -> False

    Can.TRecord fields isPartial ->
      fields & Can.fieldsToList & fmap snd & any (containsUserTypes tvarMap)

    Can.TUnit              -> False
    Can.TTuple a b mc      -> containsUserTypes tvarMap a || containsUserTypes tvarMap b || case mc of
        Just c -> containsUserTypes tvarMap c
        Nothing -> False


isAnonymousRecord :: Can.Type -> Bool
isAnonymousRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False

isRecord :: Can.Type -> Bool
isRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False

isTvar :: Can.Type -> Bool
isTvar cType =
  case cType of
    Can.TVar _ -> True
    _ -> False


-- Like == but ignores differences in alias module locations when they are pointing to equivalent types
-- Will NOT find custom types to be equivalent
isEquivalentElmType :: N.Name -> Can.Type -> Can.Type -> Bool
isEquivalentElmType debug t1 t2 = do
  -- if name /= "unchangedAllTypes"
  --   then
  --   else
      case (t1,t2) of
        (Can.TType moduleName name params, Can.TType moduleName2 name2 params2) ->

          -- debugHaskell "TType" $
            moduleName == moduleName2 && name == name2 && areEquivalentElmTypes name params params2
        (Can.TAlias moduleName name tvarMap_ aliasType, Can.TAlias moduleName2 name2 tvarMap_2 aliasType2) ->
          case (aliasType, aliasType2) of
            (Can.Holey t1, Can.Holey t2) ->
              -- debugHaskellPass "TAlias:Holey" (moduleName, moduleName2, name, name2, tvarMap_, tvarMap_2) $
              isEquivalentElmType name t1 t2 && areEquivalentTvarMaps name tvarMap_ tvarMap_2
            (Can.Filled t1, Can.Filled t2) ->
              -- debugHaskellPass "TAlias:Filled" (moduleName, moduleName2, name, name2, tvarMap_, tvarMap_2) $
              isEquivalentElmType name t1 t2 && areEquivalentTvarMaps name tvarMap_ tvarMap_2
            _ ->
              False
        (Can.TRecord fields isPartial, Can.TRecord fields2 isPartial2) ->
          -- debugHaskell "TRecord" $
          -- t1 == t2
          let
            fieldsTypes1 :: [Can.Type] = fields & Can.fieldsToList & fmap snd
            fieldTypes2 :: [Can.Type] = fields2 & Can.fieldsToList & fmap snd
          in
          (length fieldsTypes1 == length fieldTypes2)
            && areEquivalentElmTypes debug fieldsTypes1 fieldTypes2

        (Can.TTuple t1 t2 mt3, Can.TTuple t12 t22 mt32) ->
          -- debugHaskell "TTuple" $
          t1 == t2
        (Can.TUnit, Can.TUnit) ->
          -- debugHaskell "TUnit" $
          t1 == t2
        (Can.TVar name, Can.TVar name2) ->
          -- debugHaskell "TVar" $
          t1 == t2
        (Can.TLambda _ _, Can.TLambda _ _) ->
          -- Skip lambda equality not relevant
          False
        _ ->
          -- debugHaskell "unequal types" $
          False

areEquivalentElmTypes :: N.Name -> [Can.Type] -> [Can.Type] -> Bool
areEquivalentElmTypes debug types1 types2 =
  (length types1 == length types2)
    &&
  (zipWith (isEquivalentElmType debug) types1 types2 & all id)

areEquivalentTvarMaps :: N.Name -> [(N.Name, Can.Type)] -> [(N.Name, Can.Type)] -> Bool
areEquivalentTvarMaps debug tvars1 tvars2 =
  (length tvars1 == length tvars2)
    &&
  (zipWith (\(n1, t1) (n2, t2) -> n1 == n2 && isEquivalentElmType debug t1 t2) tvars1 tvars2 & all id)


-- Like == but considers identically defined union types equal despite module defs
isEquivalentEvergreenType :: Can.Type -> Can.Type -> Bool
isEquivalentEvergreenType t1 t2 =
  case (t1, t2) of
    ((Can.TType (ModuleName.Canonical (Pkg.Name "author" "project") module1) name1 params1),
     (Can.TType (ModuleName.Canonical (Pkg.Name "author" "project") module2) name2 params2)) ->
      name1 == name2 && isEquivalentEvergreenModule module1 module2
    _ -> t1 == t2

-- Like == but considers union types equal despite module defs
areEquivalentEvergreenTypes :: [Can.Type] -> [Can.Type] -> Bool
areEquivalentEvergreenTypes t1s t2s =
  let
      result =
        length t1s == length t2s
          && and (zipWith isEquivalentEvergreenType t1s t2s)
  in
  -- debugHaskellPass "areEquivalentEvergreenTypes" (result, t1s, t2s) $
  result

isEquivalentEvergreenModule :: ModuleName.Raw -> ModuleName.Raw -> Bool
isEquivalentEvergreenModule m1 m2 =
  let m1_ = m1 & N.toText & T.splitOn "."
      m2_ = m2 & N.toText & T.splitOn "."
  in
  case (m1_, m2_) of
    ("Evergreen":_:xs1, "Evergreen":_:xs2) -> xs1 == xs2
    _ -> m1 == m2



isUserModule :: ModuleName.Canonical -> Bool
isUserModule moduleName =
    case moduleName of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
            author == "author" && pkg == "project"

migrationWrapperForType :: N.Name -> Text
migrationWrapperForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelMigrated"
    "FrontendModel" -> "ModelMigrated"
    "FrontendMsg"   -> "MsgMigrated"
    "ToBackend"     -> "MsgMigrated"
    "BackendMsg"    -> "MsgMigrated"
    "ToFrontend"    -> "MsgMigrated"

migrationTypeForType :: N.Name -> Text
migrationTypeForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelMigration"
    "FrontendModel" -> "ModelMigration"
    "FrontendMsg"   -> "MsgMigration"
    "ToBackend"     -> "MsgMigration"
    "BackendMsg"    -> "MsgMigration"
    "ToFrontend"    -> "MsgMigration"

msgForType :: N.Name -> Text
msgForType t =
  case N.toChars t of
    "BackendModel"  -> "BackendMsg"
    "FrontendModel" -> "FrontendMsg"
    "FrontendMsg"   -> "FrontendMsg"
    "ToBackend"     -> "BackendMsg"
    "BackendMsg"    -> "BackendMsg"
    "ToFrontend"    -> "FrontendMsg"

unchangedForType :: N.Name -> Text
unchangedForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelUnchanged"
    "FrontendModel" -> "ModelUnchanged"
    "FrontendMsg"   -> "MsgUnchanged"
    "ToBackend"     -> "MsgUnchanged"
    "BackendMsg"    -> "MsgUnchanged"
    "ToFrontend"    -> "MsgUnchanged"


nothingTODO = Nothing


findDef :: N.Name -> N.Name -> Interfaces -> Maybe TypeDef
findDef moduleName typeName interfaces =
  case Map.lookup moduleName interfaces of
    Just moduleInterface ->
      findDef_ typeName moduleInterface

    Nothing ->
      Nothing
      -- error $ "could not find old module at all: " <> show (module_, tipe)
      -- error $ "could not find '" <> N.toChars oldModuleName <> "' old module at all: " <> show (Map.keys interfaces)

findDef_ :: N.Name -> Interface.Interface -> Maybe TypeDef
findDef_ typeName moduleInterface =
  case Map.lookup typeName $ Interface._aliases moduleInterface of
    Just aliasInterface ->
        case aliasInterface of
          Interface.PublicAlias a -> Just $ Alias a
          Interface.PrivateAlias a -> Just $ Alias a

    Nothing ->
      case Map.lookup typeName $ Interface._unions moduleInterface of
        Just unionInterface -> do
          case unionInterface of
            Interface.OpenUnion u -> Just $ Union u
            Interface.ClosedUnion u -> Just $ Union u
            Interface.PrivateUnion u -> Just $ Union u

        Nothing ->
          -- error $ "could not find old type at all: " <> show (module_, tipe)
          Nothing


asOldModuleName :: N.Name -> Int -> Int -> N.Name
asOldModuleName newModule newVersion oldVersion =
  newModule
    & N.toText
    & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ("Evergreen.V" <> show_ oldVersion <> ".")
    & N.fromText


asOldModule :: N.Name -> Int -> Int -> ModuleName.Canonical
asOldModule newModule newVersion oldVersion =
  ModuleName.Canonical (Pkg.Name "author" "project") (asOldModuleName newModule newVersion oldVersion)


tvarResolveParams :: [Can.Type] -> [(N.Name, Can.Type)] -> [Can.Type]
tvarResolveParams params tvarMap =
  params & fmap (tvarResolveParam tvarMap)

tvarResolveParam :: [(N.Name, Can.Type)] -> Can.Type -> Can.Type
tvarResolveParam tvarMap param =
  case param of
    Can.TVar a ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (_,ti) -> ti
        Nothing -> param
    _ -> param


suffixIfNonempty :: Text -> Text -> Text
suffixIfNonempty t s =
  if T.length s == 0 then s else s <> t


noMigration :: Migration
noMigration = xMigrationNested ("", Set.empty, Map.empty)
