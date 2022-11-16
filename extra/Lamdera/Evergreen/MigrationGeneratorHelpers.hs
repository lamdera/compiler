{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Evergreen.MigrationGeneratorHelpers where

import qualified AST.Canonical as Can
import qualified AST.Source as Valid
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import qualified Reporting.Error as Error

import qualified Reporting.Doc as D

import qualified System.Environment as Env
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N
import Data.Map.Strict (unionWithKey)

import qualified Data.Utf8 as Utf8

import Lamdera
import Lamdera.Types
import qualified Ext.Query.Interfaces as Interfaces
import qualified Lamdera.Progress as Progress
import qualified Ext.ElmFormat
import qualified Lamdera.Wire3.Helpers
import StandaloneInstances


type RecursionSet = Set.Set (ModuleName.Canonical, N.Name)
type TypeIdentifier = (Pkg.Author, Pkg.Project, N.Name, N.Name)
data Migration = MigrationNested
  { migrationDef :: Text
  , migrationImports :: ElmImports
  , migrationNestedDefs :: MigrationDefinitions
  , migrationName :: Text
  }

xMigrationNested (a,b,c,d) = MigrationNested a b c d


-- @TODO can we drop this given migrations generate everything inline into a single file?
data MigrationDefinition =
  MigrationDefinition
    { imports :: ElmImports
    -- , migrations :: [Text] <----- this is the problem!
    , migrations :: Map Text Text
    }
  deriving (Show, Eq)

type MigrationDefinitions = Map Text MigrationDefinition

-- Map <import name> <package>
-- @TODO in future we can use this to pin package versions and adjust import routing to those snapshots
type ElmImports = Set.Set ModuleName.Canonical


allMigrations :: MigrationDefinitions -> Text
allMigrations efts =
    efts
      & Map.toList
      & fmap (\(file, ef@(MigrationDefinition imports migrations)) -> Map.toList migrations & fmap snd )
      & List.concat
      & List.sort
      -- @TODO check if this is actually needed once we sort out incorrect gen?
      & List.nub
      & T.intercalate "\n\n"

allDefinitionImportsText :: MigrationDefinitions -> Text
allDefinitionImportsText efts =
    efts
      & Map.toList
      & fmap (\(file, ef@(MigrationDefinition imports migrations)) -> importsToText imports )
      & (++) additionalImports
      & List.concat
      & List.sort
      & List.nub
      & T.intercalate "\n"

additionalImports :: [[Text]]
additionalImports = [["import Lamdera.Migrations exposing (..)"]]

importsToText :: Set.Set ModuleName.Canonical -> [Text]
importsToText imports =
  imports
        & Set.toList
        & filterMap (\(ModuleName.Canonical (Pkg.Name author project) module_) ->
          Just $ "import " <> nameToText module_
        )
        & List.sort

lamderaCoreTypes :: [N.Name]
lamderaCoreTypes = [ "FrontendModel" , "BackendModel" , "FrontendMsg" , "ToBackend" , "BackendMsg" , "ToFrontend" ]

mergeMigrationDefinition :: Text -> MigrationDefinition -> MigrationDefinition -> MigrationDefinition
mergeMigrationDefinition k ft1 ft2 =
  MigrationDefinition
    { imports = Set.union (imports ft1) (imports ft2)
    , migrations = (migrations ft1 <> migrations ft2)
    }

allMigrationDefinitions :: [Migration] -> MigrationDefinitions
allMigrationDefinitions migrations =
  migrations & foldl (\acc migration ->
      acc
        & Map.insert
            (migrationName migration)
            (MigrationDefinition
              { imports = migrationImports migration
              , migrations = Map.singleton (migrationName migration) (migrationDef migration)
              })
        & Map.union (migrationNestedDefs migration)
  ) Map.empty


allImports :: [Migration] -> ElmImports
allImports migrations = migrations & fmap migrationImports & mergeAllImports

mergeMigrationDefinitions :: MigrationDefinitions -> MigrationDefinitions -> MigrationDefinitions
mergeMigrationDefinitions ft1 ft2 = unionWithKey mergeMigrationDefinition ft1 ft2

mergeAllMigrationDefinitions :: [MigrationDefinitions] -> MigrationDefinitions
mergeAllMigrationDefinitions ftss = ftss & foldl (\acc fts -> mergeMigrationDefinitions acc fts) Map.empty

mergeImports :: ElmImports -> ElmImports -> ElmImports
mergeImports i1 i2 = Set.union i1 i2

mergeAllImports :: [ElmImports] -> ElmImports
mergeAllImports imps = imps & foldl (\acc elmImport -> mergeImports acc elmImport) Set.empty

addImports :: ModuleName.Canonical -> ElmImports -> MigrationDefinitions -> MigrationDefinitions
addImports scope@(ModuleName.Canonical (Pkg.Name author pkg) module_) imports ft =
  imports & foldl (\ft imp -> addImport scope imp ft) ft

addImport :: ModuleName.Canonical -> ModuleName.Canonical -> MigrationDefinitions -> MigrationDefinitions
addImport moduleName imp ft =
  ft
    & Map.alter (\mft ->
      case mft of
        Just ft ->
          Just $ ft { imports = imports ft & Set.insert imp }

        Nothing ->
          Just $
            MigrationDefinition
              { imports = Set.singleton imp
              , migrations = Map.empty
              }
    ) (moduleNameKey moduleName)


migrationNameUnderscored :: N.Name -> Int -> Int -> N.Name -> Text
migrationNameUnderscored newModule oldVersion newVersion newTypeName =
  newModule
    & N.toText
    & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ""
    & T.replace ("Evergreen.V" <> show_ oldVersion <> ".") ""
    & T.replace "." "_"
    & (\v -> "migrate_" <> v <> "_" <> nameToText newTypeName)


data TypeDef = Alias Can.Alias | Union Can.Union deriving (Show)


-- basicUnimplemented :: Maybe Can.Type -> Can.Type -> Migration
-- basicUnimplemented tipeOldM tipe =
--   case tipeOldM of
--     Just tipeOld ->
--       if tipeOld /= tipe then
--         ("Unimplemented -- expecting: " <> (qualifiedTypeName tipeOld) <> " -> " <> qualifiedTypeName tipe, Set.empty, Map.empty)
--       else
--         ("UHHHH1", Set.empty, Map.empty)
--     Nothing ->
--       ("UHHHH2", Set.empty, Map.empty)


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

    -- _ -> error $ "asIdentifierUnimplemented: " <> show tipe


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
    Can.TRecord _ _ -> "ANONYMOUSERECORD"
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

isAnonymousRecord :: Can.Type -> Bool
isAnonymousRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False


isUserModule :: ModuleName.Canonical -> Bool
isUserModule moduleName =
    case moduleName of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
            author == "author" && pkg == "project"

migrationWrapperForType :: Text -> Text
migrationWrapperForType t =
  case t of
    "BackendModel"  -> "ModelMigration"
    "FrontendModel" -> "ModelMigration"
    "FrontendMsg"   -> "MsgMigration"
    "ToBackend"     -> "MsgMigration"
    "BackendMsg"    -> "MsgMigration"
    "ToFrontend"    -> "MsgMigration"

msgForType :: Text -> Text
msgForType t =
  case t of
    "BackendModel"  -> "BackendMsg"
    "FrontendModel" -> "FrontendMsg"
    "FrontendMsg"   -> "FrontendMsg"
    "ToBackend"     -> "BackendMsg"
    "BackendMsg"    -> "BackendMsg"
    "ToFrontend"    -> "FrontendMsg"

unchangedForType :: Text -> Text
unchangedForType t =
  case t of
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



tvarResolvedParams :: [Can.Type] -> [(N.Name, Can.Type)] -> [Can.Type]
tvarResolvedParams params tvarMap =
  params
    & fmap (\p ->
      case p of
        Can.TVar a ->
          case List.find (\(t,ti) -> t == a) tvarMap of
            Just (_,ti) -> ti
            Nothing -> p
        _ -> p
    )