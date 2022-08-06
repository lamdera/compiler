{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Evergreen.MigrationGenerator where

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
import qualified Lamdera.Compile
import qualified Ext.Query.Interfaces as Interfaces
import qualified Lamdera.Progress as Progress
import qualified Ext.ElmFormat
import qualified Ext.Query.Interfaces
import qualified Lamdera.Wire3.Helpers
import StandaloneInstances


import Lamdera.Evergreen.MigrationGeneratorUnion
import Lamdera.Evergreen.MigrationGeneratorAlias
import Lamdera.Evergreen.MigrationGeneratorHelpers


betweenVersions :: Int -> Int -> IO Text
betweenVersions oldVersion newVersion = do
    let
        -- @TODO generalise params
        project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"
        paths = ["src/Evergreen/V" <> show oldVersion <> "/Types.elm", "src/Evergreen/V" <> show newVersion <> "/Types.elm"]

    Lamdera.Compile.makeDev project paths

    res <- withCurrentDirectory project $ do
        interfaces <- Ext.Query.Interfaces.all paths
        generateFor oldVersion newVersion interfaces (interfaces Map.! (N.fromChars $ "Evergreen.V" <> show newVersion <> ".Types"))

    Ext.ElmFormat.formatOrPassthrough res


generateFor :: Int -> Int -> Interfaces -> Interface.Interface -> IO Text
generateFor oldVersion newVersion interfaces iface_Types = do
  -- pure ""
  let
    moduleName :: ModuleName.Canonical
    moduleName = (ModuleName.Canonical (Pkg.Name "author" "project") (N.fromChars $ "Evergreen.V" <> show newVersion <> ".Types"))

    efts :: MigrationDefinitions
    efts =
      lamderaTypes
        & fmap (\t -> (t, ftByName oldVersion newVersion interfaces moduleName t iface_Types))
        & foldl (\acc (t, ft) -> mergeMigrationDefinitions acc ft) Map.empty
        -- a little weird but ensures current version of types, our entry point, is added to final migration imports...
        & addImport moduleName moduleName

    -- debugEfts = efts & eftToText version

  pure $ ("module Evergreen.Migrate.V" <> show_ newVersion <> " exposing (..)\n\n")
    <> allDefinitionImportsText efts
    <> "\n\n"
    <> allMigrations efts


ftByName :: Int -> Int -> Interfaces -> ModuleName.Canonical -> N.Name -> Interface.Interface -> MigrationDefinitions
ftByName oldVersion newVersion interfaces newModule typeName interface = do
  let
    recursionIdentifier :: (ModuleName.Canonical, N.Name)
    recursionIdentifier = (newModule, typeName)
    recursionSet :: RecursionSet
    recursionSet = Set.singleton recursionIdentifier
    identifier :: TypeIdentifier
    identifier = asIdentifier_ recursionIdentifier

  case findDef (canModuleName newModule) typeName interfaces of
    Just (Alias alias) -> do
      let
        diffableAlias = aliasToFt oldVersion newVersion newModule identifier typeName interfaces recursionSet alias
        (subt, imps, subft) = diffableAlias
      subft & addImports newModule imps

    Just (Union union) -> do
      let
        diffableUnion = unionToFt oldVersion newVersion newModule identifier typeName interfaces recursionSet [] union []
        (subt, imps, subft) = diffableUnion
      subft & addImports newModule imps

    Nothing -> Map.empty


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToFt :: Int -> Int -> ModuleName.Canonical -> TypeIdentifier -> N.Name -> Interfaces -> RecursionSet -> [(N.Name, Can.Type)] -> Can.Union -> [Can.Type] -> Migration
unionToFt oldVersion newVersion scope identifier@(author, pkg, newModule, tipe) typeName interfaces recursionSet tvarMap newUnion params =
  let
    oldModuleName :: N.Name
    oldModuleName = asOldModuleName newModule newVersion oldVersion

    tipeOld :: Maybe TypeDef
    tipeOld = findDef oldModuleName typeName interfaces
  in
  case tipeOld of
  Nothing        -> ("Unimplemented -- Cannot find any old type with same name as new type", Set.empty, Map.empty)
  Just (Alias a) -> ("Unimplemented -- Old type was an Custom type, new type is an Alias", Set.empty, Map.empty)
  Just (Union oldUnion) ->
    migrateUnion author pkg oldUnion newUnion params tvarMap oldVersion newVersion typeName newModule identifier oldModuleName interfaces recursionSet scope


-- migrateUnion :: Can.Union -> Can.Union -> Migration
migrateUnion author pkg oldUnion newUnion params tvarMap oldVersion newVersion typeName newModule identifier oldModuleName interfaces recursionSet scope =
  let
    oldModuleNameCanonical :: ModuleName.Canonical
    oldModuleNameCanonical =
      (ModuleName.Canonical (Pkg.Name "author" "project") oldModuleName)

    tvarMap :: [(N.Name, Can.Type)]
    tvarMap =
      zip (Can._u_vars newUnion) params

    tvars_ :: Text
    tvars_ =
      tvarMap
        & fmap (N.toText . fst)
        & T.intercalate " "

    paramMigrations :: [Migration]
    paramMigrations =
      tvarResolvedParams params tvarMap
        & fmap (\param -> canonicalToFt oldVersion newVersion scope interfaces recursionSet param nothingTODO tvarMap)

    paramMigrationTextsCombined :: Text
    paramMigrationTextsCombined =
      paramMigrations
        & fmap selectMigrationText
        & T.intercalate " "

    usageImports :: Set.Set ModuleName.Canonical
    usageImports = paramMigrations & allImports & Set.insert oldModuleNameCanonical

    paramMigrationDefinitions :: MigrationDefinitions
    paramMigrationDefinitions = paramMigrations & allMigrationDefinitions

    localScope :: ModuleName.Canonical
    localScope =
      (ModuleName.Canonical (Pkg.Name author pkg) newModule)

    oldConstructorsMigrations :: [Migration]
    oldConstructorsMigrations = genOldConstructorMigrationDefinitions oldModuleName moduleScope typeName interfaces tvarMap recursionSet localScope newVersion oldVersion newUnion oldUnion

    imports :: ElmImports
    imports = oldConstructorsMigrations & foldl (\acc (st, imps, ft) -> mergeImports acc imps) Set.empty

    oldConstructorsMigrationDefinitions :: MigrationDefinitions
    oldConstructorsMigrationDefinitions = oldConstructorsMigrations & foldl (\acc (st, imps, ft) -> mergeMigrationDefinitions acc ft) Map.empty

    moduleScope :: Text
    moduleScope = nameToText newModule <> "."

    moduleScopeOld :: Text
    moduleScopeOld = nameToText oldModuleName <> "."

    -- debug (t, imps, ft) =
    --   -- debugHaskellWhen (typeName == "RoomId") ("dunion: " <> hindentFormatValue scope) (t, imps, ft)
    --   debugNote ("\n✴️  inserting def for " <> t) (t, imps, ft)

    migrationName :: Text
    migrationName = migrationNameUnderscored newModule oldVersion newVersion typeName

    migration :: Text
    migration = migrationName <> " " <> paramMigrationTextsCombined -- <> "<!2>"

    migrationTypeSignature :: Text
    migrationTypeSignature = T.concat [ oldModuleName & N.toText, ".", typeName & N.toText, " -> ", newModule & N.toText, ".", typeName & N.toText]

    constructorCaseMigrations :: Text
    constructorCaseMigrations =
      oldConstructorsMigrations
        & fmap selectMigrationText
        & flip (++) (newConstructorWarnings typeName moduleScopeOld newUnion oldUnion)
        & T.concat

  in
  -- debug $
  ( migration
  , usageImports
  , (Map.singleton (moduleKey identifier) $
      MigrationDefinition
        { imports = imports
        , types = [ T.concat
                    [ migrationName, " : ", migrationTypeSignature, "\n"
                    , migrationName <> " old =\n"
                    , "  case old of\n"
                    , constructorCaseMigrations
                    ]
                  ]
        })
      & mergeMigrationDefinitions oldConstructorsMigrationDefinitions
      & mergeMigrationDefinitions paramMigrationDefinitions
  )


-- genOldConstructorMigrationDefinitions :: [Migration]
genOldConstructorMigrationDefinitions oldModuleName moduleScope typeName interfaces tvarMap recursionSet localScope newVersion oldVersion newUnion oldUnion =
  -- error "tbc"
  Can._u_alts oldUnion
    & fmap (\(Can.Ctor oldConstructor index int oldParams) ->
      -- For each OLD constructor type param
      genOldConstructorFt oldModuleName moduleScope typeName interfaces tvarMap recursionSet localScope newVersion oldVersion newUnion oldUnion oldConstructor oldParams

    )


genOldConstructorFt oldModuleName moduleScope typeName interfaces tvarMap recursionSet localScope newVersion oldVersion newUnion oldUnion oldConstructor oldParams =
  let
    cparams :: [Migration]
    cparams =
      -- @TODO we need to include new params here as well?
      -- yes – but we get them from the paramMigrations instead.
      -- so @TODO remove this block entirely once rewritten
      oldParams &
        fmap (\param ->
          canonicalToFt oldVersion newVersion localScope interfaces recursionSet param nothingTODO tvarMap
          )

    newCtorM :: Maybe Can.Ctor
    newCtorM = Can._u_alts newUnion & List.find (\(Can.Ctor newConstructor _ _ _) -> newConstructor == oldConstructor )


    migration_ :: Migration
    migration_ =
      case newCtorM of
      Just (Can.Ctor newConstructor _ _ newParams) ->
        let
            paramMigrations :: [Migration]
            paramMigrations =
              zipFull oldParams newParams
                & imap (\i (paramOldM, paramNewM) ->
                  case (paramOldM, paramNewM) of
                    (Just paramOld, Just paramNew) ->
                      let ft@(migration, imps, subft) =
                            canonicalToFt oldVersion newVersion localScope interfaces recursionSet paramNew (Just paramOld) tvarMap

                          appliedMigration =
                            if paramOld == paramNew then
                              T.concat ["p", show_ i]
                            else if isAnonymousRecord paramOld then
                              migration
                            else if isUserDefinedType_ paramOld then
                              -- T.concat [" (", migrationNameUnderscored oldModuleName oldVersion newVersion (N.fromText $ asTypeName paramOld),  " p", show_ i, ")"]
                              T.concat ["(p", show_ i, " |> ", migration, ")"]
                            else
                              -- T.concat ["p", show_ i]
                              T.concat ["(p", show_ i, " |> ", migration, ")"]

                      in
                      (appliedMigration, imps, subft)
                      -- migration
                      -- <> "MARKER🟠"

                    (Just paramOld, Nothing) ->
                      ("\n-- warning: old variant didn't get mapped to anything, check this is what you want\n", Set.empty, Map.empty)

                    (Nothing, Just paramNew) ->
                      ("(Debug.todo \"this new variant needs to be initialised!\")", Set.empty, Map.empty)

                    _ -> error "impossible, zip produced a value without any contents"

                )

            fullMigration =
              ( if List.length oldParams > 0 then
                  let migration =
                        paramMigrations
                          & fmap selectMigrationText
                          & (\paramMigrations -> T.concat [ moduleScope, N.toText newConstructor, " ", (paramMigrations & T.intercalate " ") ])
                  in
                  T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " ", (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " "), " -> ", migration, "\n"]
                else
                  T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " -> ", moduleScope, N.toText newConstructor, "\n"]
              , paramMigrations
                  & fmap selectImports
                  & mergeAllImports
              , paramMigrations
                  & fmap selectMigrationDefinitions
                  & mergeAllMigrationDefinitions
              )
        in
        fullMigration

      Nothing ->
        ( -- No old constructor with same name, so this is a new/renamed constructor
          if List.length oldParams > 0
            then
              T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " ", (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " "), (oldConstructorRemovedMessage oldConstructor moduleScope typeName oldModuleName)]
            else
              T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, (oldConstructorRemovedMessage oldConstructor moduleScope typeName oldModuleName)]
        , Set.empty
        , Map.empty
        )


    -- migration :: Text
    -- migration =
    --   case newCtorM of
    --   Just (Can.Ctor newConstructor _ _ newParams) ->
    --       let
    --         paramsZipped :: [Text]
    --         paramsZipped =
    --           zipFull oldParams newParams
    --             & imap (\i (paramOldM, paramNewM) ->
    --               case (paramOldM, paramNewM) of
    --                 (Just paramOld, Just paramNew) ->
    --                   let ft@(migration, imps, subft) =
    --                         canonicalToFt oldVersion newVersion localScope interfaces recursionSet paramOld (Just paramNew) tvarMap
    --                   in
    --                   -- migration
    --                   -- <> "MARKER🟠"

    --                   if paramOld == paramNew then
    --                     T.concat ["p", show_ i]
    --                   else if isAnonymousRecord paramOld then
    --                     migration
    --                   else if isUserDefinedType_ paramOld then
    --                     -- T.concat [" (", migrationNameUnderscored oldModuleName oldVersion newVersion (N.fromText $ asTypeName paramOld),  " p", show_ i, ")"]
    --                     T.concat [" (", migration,  " p", show_ i, ")"]
    --                   else
    --                     -- T.concat ["p", show_ i]
    --                     T.concat [" (", migration,  " p", show_ i, ")"]

    --                 (Just paramOld, Nothing) -> "\n-- warning: old variant didn't get mapped to anything, check this is what you want\n"

    --                 (Nothing, Just paramNew) -> "(Debug.todo \"this new variant needs to be initialised!\")"

    --                 _ -> error "impossible, zip produced a value without any contents"

    --             )

    --         migration :: Text
    --         migration =
    --           -- if areEquivalentEvergreenTypes oldParams newParams then
    --             T.concat [ moduleScope, N.toText newConstructor, " ", (paramsZipped & T.intercalate " ") ]
    --           -- else
    --             -- "-- params have changed\nUnimplemented"
    --       in
    --       if List.length oldParams > 0 then
    --         T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " ", (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " "), " -> ", migration, "\n"]
    --       else
    --         T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " -> ", moduleScope, N.toText newConstructor, "\n"]

    --   Nothing -> do
    --     -- No old constructor with same name, so this is a new/renamed constructor
    --     if List.length oldParams > 0
    --       then
    --         T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, " ", (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " "), (oldConstructorRemovedMessage oldConstructor moduleScope typeName oldModuleName)]
    --       else
    --         T.concat ["    ", N.toText oldModuleName, ".", N.toText oldConstructor, (oldConstructorRemovedMessage oldConstructor moduleScope typeName oldModuleName)]

  in
  migration_
  -- ( migration
  -- , foldl (\acc (st, imps, ft) -> mergeImports acc imps) Set.empty cparams
  -- , foldl (\acc (st, imps, ft) -> mergeMigrationDefinitions acc ft) Map.empty cparams
  -- )

-- oldConstructorRemovedMessage :: _ -> Text
oldConstructorRemovedMessage oldConstructor moduleScope typeName oldModuleName =
  T.concat [
    " ->\n",
    "           {- `", N.toText oldConstructor, "` doesn't exist in ", moduleScope, N.toText typeName, " so I couldn't figure out how to migrate it.\n",
    "           You'll need to decide what happens to this ", N.toText oldModuleName, ".", N.toText oldConstructor, " value in a migration.\n",
    "           See https://lamdera.com/tips/modified-custom-type for more info. -}\n",
    "           Unimplemented\n"
  ]




-- Like == but considers union types equal despite module defs
areEquivalentEvergreenTypes t1s t2s =
  let
      result =
        length t1s == length t2s
          && and (zipWith isEquivalentEvergreenType t1s t2s)
  in
  -- debugHaskellPass "areEquivalentEvergreenTypes" (result, t1s, t2s) $
  result

isEquivalentEvergreenType t1 t2 =
  case (t1, t2) of
    ((Can.TType (ModuleName.Canonical (Pkg.Name "author" "project") module1) name1 params1),
     (Can.TType (ModuleName.Canonical (Pkg.Name "author" "project") module2) name2 params2)) ->
      name1 == name2 && isEquivalentEvergreenModule module1 module2
    _ -> t1 == t2

isEquivalentEvergreenModule m1 m2 =
  let m1_ = m1 & N.toText & T.splitOn "."
      m2_ = m2 & N.toText & T.splitOn "."
  in
  case (m1_, m2_) of
    ("Evergreen":_:xs1, "Evergreen":_:xs2) -> xs1 == xs2
    _ -> m1 == m2


-- A top level Alias definition i.e. `type alias ...`
aliasToFt :: Int -> Int -> ModuleName.Canonical -> TypeIdentifier -> N.Name -> Interfaces -> RecursionSet -> Can.Alias -> Migration
aliasToFt oldVersion newVersion scope identifier@(author, pkg, newModule, _) typeName interfaces recursionSet (Can.Alias tvars tipe) =
  let
    oldModuleName :: N.Name
    oldModuleName = asOldModuleName newModule newVersion oldVersion

    tipeOld :: Maybe TypeDef
    tipeOld = findDef oldModuleName typeName interfaces

  in
  case tipeOld of
    (Just (Alias (Can.Alias tvarsOld tipeOld))) ->
      let
        (subt, imps, subft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe (Just tipeOld) []

        tvars_ = tvars & fmap N.toText & T.intercalate " "

        typeScope =
          if moduleName == scope then
            ""
          -- else if isUserType identifier then
          --   nameToText newModule <> "."
          else
            T.concat [nameToText newModule, "."]

        moduleName = (ModuleName.Canonical (Pkg.Name author pkg) newModule)

        migration =
          if tipe == tipeOld then
            "ModelUnchanged"
          else
            subt

        newModuleName =
          newModule
            & N.toText

      in
      ( if length tvars > 0 then
          T.concat ["(", typeScope, nameToText typeName, tvars_, ")"] -- <> "<!2>"
        else
          T.concat [typeScope, nameToText typeName] -- <> "<!3>"
      , imps
      , (Map.singleton (moduleKey identifier) $
          MigrationDefinition
            { imports = imps
            , types = [ typedAliasMigration oldModuleName newModuleName typeName migration ]
            })
          & mergeMigrationDefinitions subft
      )
    _ -> error "old alias became ???"

typedAliasMigration oldModuleName newModuleName typeName migration =
  T.concat ["\n", (lowerFirstLetter_ $ nameToText typeName), " : ", nameToText oldModuleName, ".", nameToText typeName, " -> ModelMigration ", newModuleName, ".", nameToText typeName, " ", newModuleName, ".", msgForType (nameToText typeName), "\n",
  (lowerFirstLetter_ $ nameToText typeName), " old = ", migration]

canonicalToFt :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> Migration
canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap =
  let
    -- scopeModule =
    --   case scope of
    --     (ModuleName.Canonical (Pkg.Name author pkg) module_) -> module_

    -- debug (t, imps, ft) =
    --   -- debugHaskellWhen (textContains "OptionalData" t) ("\n✳️  inserting def for " <> t <> "\n" <> (T.pack . show $ canonical)) (t, imps, ft)
    --   -- debug_note ("🔵inserting def for " <> T.unpack t <> ":\n" <> ( ft)) $ (t, imps, ft)
    --   unsafePerformIO $ do
    --       formatHaskellValue ("\n🔵inserting def for " <> (show_ tipeOldM)) (t, ft) :: IO ()
    --       -- formatHaskellValue ("\n🟠 which had oldtype: " <> t) (tipeOldM) :: IO ()
    --       pure (t, imps, ft)
  in
  -- debug $
  case tipe of
    Can.TType moduleName name params -> handleTypeToFt oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap

    Can.TAlias moduleName name tvarMap_ aliasType -> handleAliasToFt oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap

    Can.TRecord newFields isPartial -> handleRecordToFt oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap

    Can.TTuple t1 t2 mt3 ->
      let
        (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet t1 nothingTODO tvarMap)
        (subt2, imps2, subft2) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet t2 nothingTODO tvarMap)
      in
      case mt3 of
        Just t3 ->
          let
            (subt3, imps3, subft3) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet t3 nothingTODO tvarMap)
          in
          (T.concat ["(", subt, ", ", subt2, ", ", subt3, ")"], mergeAllImports [imps,imps2,imps3], mergeAllMigrationDefinitions [subft,subft2,subft3])

        Nothing ->
          (T.concat ["(", subt, ", ", subt2, ")"], mergeImports imps imps2, mergeMigrationDefinitions subft subft2)
      -- DTuple (canonicalToFt oldVersion newVersion scope interfaces recursionSet t1 tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet t2 tvarMap)

    Can.TUnit ->
      ("()", Set.empty, Map.empty)

    Can.TVar name ->
      (N.toText name, Set.empty, Map.empty)

    Can.TLambda _ _ ->
      error "Fatal: impossible function type! Please report this gen issue."
      -- ("XXXXXX TLambda", Set.empty, Map.empty)
      -- DError $ "must not contain functions"


handleAliasToFt :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> Migration
handleAliasToFt oldVersion newVersion scope interfaces recursionSet tipe@(Can.TAlias moduleName name tvarMap_ aliasType) tipeOldM tvarMap =
  let
    module_ =
      case moduleName of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) -> module_

    newModule = module_

    identifier = asIdentifier_ (moduleName, name)

    oldModuleName :: N.Name
    oldModuleName = asOldModuleName newModule newVersion oldVersion

    typeName = name
  in
  case aliasType of
    Can.Holey cType ->
      let
        usageParamFts :: [Migration]
        usageParamFts =
          tvarMap_
            & fmap (\(n, paramType) ->
              canonicalToFt oldVersion newVersion scope interfaces recursionSet paramType nothingTODO tvarMap_
            )

        usageParamNames :: Text
        usageParamNames =
          usageParamFts
            & fmap selectMigrationText
            & T.intercalate " "

        usageParamImports :: ElmImports
        usageParamImports =
          usageParamFts
            & fmap selectImports
            & mergeAllImports

        tvars :: Text
        tvars =
          tvarMap_
            & fmap (N.toText . fst)
            & T.intercalate " "

        (subt, imps, subft) :: Migration =
          case tipeOldM of
            Just tipeOld ->
              canonicalToFt oldVersion newVersion moduleName interfaces recursionSet cType (Just tipeOld) tvarMap_
            Nothing ->
              canonicalToFt oldVersion newVersion moduleName interfaces recursionSet cType nothingTODO tvarMap_

        typeScope =
          if moduleName == scope then
            ""
          -- else if isUserType identifier then
          --   nameToText module_ <> "."
          else
            T.concat [nameToText module_, "."]

        debugIden = "" -- <> "<ah>"

        scopeImports =
          -- if moduleName == scope then
          --   usageParamImports
          -- else
            usageParamImports & Set.insert moduleName

        migrationName :: Text
        migrationName = migrationNameUnderscored newModule oldVersion newVersion name

        migrationTypeSignature :: Text
        migrationTypeSignature = T.concat [ oldModuleName & N.toText, ".", typeName & N.toText, " -> ", newModule & N.toText, ".", typeName & N.toText]

        typeDef =
          if isUserDefinedType_ cType then
            -- if length tvarMap_ > 0 then
            --     [migrationName <> " old = " <> subt]
            -- else
                [ T.concat
                  [ migrationName, " : ", migrationTypeSignature, "\n"
                  , migrationName, " old = ", subt
                  ]
                ]
          else
            -- ["-- no migration for primitive: " <> N.toText name ]
            []

        thing =
          (Map.singleton (moduleNameKey moduleName) $
                MigrationDefinition
                  { imports = imps
                  , types = typeDef
                  })
                & mergeMigrationDefinitions subft
                & mergeMigrationDefinitions (mergeAllMigrationDefinitions (fmap selectMigrationDefinitions usageParamFts))


        migration =
          if length tvarMap_ > 0 then
            T.concat ["(", migrationName, " ", usageParamNames, ")"] -- <> "<!2>"
          else
            migrationName -- <> "<!3>"


        -- !_ = formatHaskellValue "Can.TAlias.Holey:" (name, cType, tvarMap_, moduleName, scope) :: IO ()
      in
      -- debug_note ("🔵inserting def for " <> T.unpack (moduleNameKey moduleName) <> "." <> N.toString name <> "\n" <> (T.unpack $ head typeDef)) $

      case tipeOldM of
        Just tipeOld ->
          if tipeOld == tipe then
            ("SAMETYPES", scopeImports, thing)
          else
            (
              -- debugIden <> "🔵" <>
              migration
              -- "migrateeee" <>
              -- if length tvarMap_ > 0 then
              --   "(" <> typeScope <> N.toText name <> " " <> usageParamNames <> ")"
              -- else
              --   typeScope <> N.toText name
            , scopeImports
            , thing
            )
        _ -> (T.concat ["-- TODO old type gone for type: ", show_ tipe], scopeImports, thing)


    Can.Filled cType ->
      -- @TODO hypothesis...
      -- If an alias is filled, then it can't have any open holes within it either?
      -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
      let
        (subt, imps, subft) = canonicalToFt oldVersion newVersion moduleName interfaces recursionSet cType nothingTODO []

        debugIden = "" -- <> "<af>"
      in
      (
        debugIden <> "🔴" <>
        if module_ == (canModuleName scope) then
          N.toText name
        else if isUserType identifier then
          T.concat [nameToText module_, "."]
        else
          T.concat [nameToText module_, ".", N.toText name]
      , imps
      , (Map.singleton (moduleNameKey moduleName) $
          MigrationDefinition
            { imports = imps
            , types = [T.concat["1type alias ", N.toText name, " = ", subt]]
            })
          & mergeMigrationDefinitions subft
      )


handleRecordToFt :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> Migration
handleRecordToFt oldVersion newVersion scope interfaces recursionSet tipe@(Can.TRecord newFields isPartial) tipeOldM tvarMap =
  case isPartial of
    Just whatIsThis ->
      ("ERROR TRecord, please report this!", Set.empty, Map.empty)
      -- DError "must not contain partial records"

    Nothing ->
      let
        fieldMapOld :: Map N.Name Can.FieldType
        fieldMapOld =
          case tipeOldM of
            Just tipeOld ->
              case Lamdera.Wire3.Helpers.resolveFieldMap tipeOld tvarMap of
                Just fields -> fields
                Nothing -> Map.empty
            _ -> Map.empty

        fields :: [(Text, Migration)]
        fields =
          newFields
            & Map.toList
            -- Restore user's field code-ordering to keep types looking familiar
            & List.sortOn (\(name, (Can.FieldType index tipe)) -> index)
            & fmap (\(name, (Can.FieldType index tipe)) ->
                case Map.lookup name fieldMapOld of
                  Just (Can.FieldType index_ tipeOld) ->
                    if tipeOld == tipe then
                      (N.toText name, (T.concat["old.", N.toText name], Set.empty, Map.empty))
                    else
                      -- @TODO NEXT
                      -- this shoudl generate a migrate<NewTypeName> function – but maybe that comes from the canonicalToFt?
                      -- at the least we can thread the tipeOld type in here now!
                      let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe (Just (tipeOld)) tvarMap
                      in
                      (N.toText name, (T.concat["old.", N.toText name, " |> ", st], imps, ft))
                      -- (N.toText name, canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap)

                  Nothing ->
                    -- This field did not exist in the old version. We need an init! @TODO
                    let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap
                    in
                    ( N.toText name, (T.concat["Unimplemented -- Type `", qualifiedTypeName tipe, "` added in V", show_ newVersion, ". I need you to set a default value."], imps, ft) )
            )
            & (\v -> v ++ missingFields)

        missingFields :: [(Text, Migration)]
        missingFields =
          fieldMapOld
            & Map.toList
            & filterMap (\(name, (Can.FieldType index tipe)) ->
              case Map.lookup name newFields of
                Just (Can.FieldType index_ tipeOld) ->
                  Nothing
                Nothing ->
                  -- This seemed like a nice idea to add the migrations anyway, but what does it mean to add a migration for an old type
                  -- that has no equivalent new type in the current context...?
                  -- let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap
                  -- in
                  Just ( N.toText name,
                    (T.concat["Unimplemented -- Type `", qualifiedTypeName tipe, "` removed in V", show_ newVersion, ". Make sure to do something with the `old.", N.toText name, "` value if you wish to keep the data, then remove this notice."]
                    , Set.empty
                    , Map.empty)
                    )
            )

        fieldsFormatted :: Text
        fieldsFormatted =
          fields
            & fmap (\(fieldname, (st, imps, ft)) -> fieldname <> " = " <> st)
            & T.intercalate "\n    , "

        imports :: ElmImports
        imports =
          fields
            & foldl (\acc (name, (st, imps, ft)) -> mergeImports acc imps) Set.empty

        mergedFt :: MigrationDefinitions
        mergedFt =
          fields
            & foldl (\acc (name, (st, imps, ft)) -> mergeMigrationDefinitions acc ft) Map.empty
            & addImports scope imports

        result :: Migration
        result =
          ("\n    { " <> fieldsFormatted <> "\n    }"
          , imports
          , mergedFt
          )
      in
      result


handleTypeToFt :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> Migration
handleTypeToFt oldVersion newVersion scope interfaces recursionSet tipe@(Can.TType moduleName name params) tipeOldM tvarMap =
  -- error "tbc"
  let
    recursionIdentifier :: (ModuleName.Canonical, N.Name)
    recursionIdentifier = (moduleName, name)

    newRecursionSet :: Set.Set (ModuleName.Canonical, N.Name)
    newRecursionSet = Set.insert recursionIdentifier recursionSet

    identifier :: TypeIdentifier
    identifier = asIdentifier tipe

    typeName :: Text
    typeName = asTypeName tipe

    kernelError :: Migration
    kernelError =
      case identifier of
        (author, pkg, module_, typeName) ->
          ("XXXXXX Kernel error", Set.empty, Map.empty)
          -- DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> nameToText module_

    moduleNameRaw :: N.Name
    moduleNameRaw = canModuleName moduleName

    migrateSingleParamCollection =
      case tipeOldM of
        Just tipeOld ->
          case tipeOld of
            Can.TType _ _ paramsOld -> -- @WARNING MUST BE SAME TYPE?
              zipFull paramsOld params & (\p ->
                case p of
                  (Just p1o,Just p1):_ ->
                    let
                      (migration1, imps1, subft1) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p1 (Just p1o) tvarMap)

                      migration = case (p1o == p1) of
                        (True) -> "" -- No migration necessary
                        (False) ->
                          if migration1 == ""
                            then
                              T.concat [ typeName, ".map Unimplemented", " -- Type changed from `", typeName, " ", qualifiedTypeName p1o, "` to `", typeName, " ", qualifiedTypeName p1, "`\n" ]
                            else
                              T.concat [ typeName, ".map ", migration1 ]
                    in
                    (migration, imps1 & Set.insert moduleName, subft1)
                  _ ->
                    error $ T.unpack $ T.concat ["Fatal: impossible multi-param ", typeName, "! Please report this gen issue."]
              )
            _ -> error $ T.unpack $ T.concat ["non-matching TType in ", typeName, " gen"]

        Nothing ->
          (T.concat ["Unimplemented -- This ", typeName, " type has changed to something else"], Set.empty, Map.empty)

  in
  if (Set.member recursionIdentifier recursionSet) then
    handleSeenRecursiveType  oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap
  else
  case identifier of
    ("elm", "core", "String", "String") -> ("", Set.empty, Map.empty)
    ("elm", "core", "Basics", "Int") ->    ("", Set.empty, Map.empty)
    ("elm", "core", "Basics", "Float") ->  ("", Set.empty, Map.empty)
    ("elm", "core", "Basics", "Bool") ->   ("", Set.empty, Map.empty)
    ("elm", "core", "Basics", "Order") ->  ("", Set.empty, Map.empty)
    ("elm", "core", "Basics", "Never") ->  ("", Set.empty, Map.empty)
    ("elm", "core", "Char", "Char") ->     ("", Set.empty, Map.empty)

    ("elm", "core", "Maybe", "Maybe") -> migrateSingleParamCollection
    ("elm", "core", "List", "List") ->   migrateSingleParamCollection
    ("elm", "core", "Array", "Array") -> migrateSingleParamCollection
    ("elm", "core", "Set", "Set") ->     migrateSingleParamCollection

    ("elm", "core", "Result", "Result") ->
      case tipeOldM of
        Just tipeOld ->
          case tipeOld of
            Can.TType _ _ paramsOld -> -- @WARNING MUST BE SAME TYPE?
              zipFull paramsOld params & (\p ->
                case p of
                  (Just p1o,Just p1):(Just p2o,Just p2):_ ->
                    let
                      (subt1, imps1, subft1) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p1 (Just p1o) tvarMap)
                      (subt2, imps2, subft2) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p2 (Just p2o) tvarMap)

                      migration = case (p1o == p1, p2o == p2) of
                        (True, True) -> "" -- No migration necessary
                        (False, True) -> T.concat [ "Result.mapError ", subt1]
                        (True, False) -> T.concat [ "Result.map ", subt2 ]
                        (False, False) -> T.concat [ "Result.mapError ", subt1, " |> Result.map ", subt2 ]
                    in
                    (migration, mergeImports imps1 imps2 & Set.insert moduleName, mergeMigrationDefinitions subft1 subft2)
                    -- DResult (canonicalToFt oldVersion newVersion scope interfaces recursionSet result tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet err tvarMap)
                  _ ->
                    error "Fatal: impossible !2 param Result type! Please report this gen issue."
              )
            _ -> error "non-matching TType in Result gen"

        Nothing ->
          ("Unimplemented -- This Result type has changed to something else", Set.empty, Map.empty)


    ("elm", "core", "Dict", "Dict") ->
      case tipeOldM of
        Just tipeOld ->
          case tipeOld of
            Can.TType _ _ paramsOld -> -- @WARNING MUST BE SAME TYPE?
              zipFull paramsOld params & (\p ->
                case p of
                  (Just p1o,Just p1):(Just p2o,Just p2):_ ->
                    let
                      (subt1, imps1, subft1) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p1 (Just p1o) tvarMap)
                      (subt2, imps2, subft2) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p2 (Just p2o) tvarMap)

                      migration = case (p1o == p1, p2o == p2) of
                        (True, True) -> "" -- No migration necessary
                        (False, True) -> T.concat [ "Dict.toList |> List.map (Tuple.mapFirst ", subt1, ") |> Dict.fromList" ]
                        (True, False) -> T.concat [ "Dict.map (\\k v -> v |> ", subt2, ")" ]
                        (False, False) -> T.concat [ "Dict.toList |> List.map (Tuple.mapBoth ", subt1, " ", subt2, ") |> Dict.fromList" ]
                    in
                    (migration, mergeImports imps1 imps2 & Set.insert moduleName, mergeMigrationDefinitions subft1 subft2)
                    -- DResult (canonicalToFt oldVersion newVersion scope interfaces recursionSet result tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet err tvarMap)
                  _ ->
                    error "Fatal: impossible !2 param Result type! Please report this gen issue."
              )
            _ -> error "non-matching TType in Dict gen"
        Nothing ->
          ("Unimplemented -- This Dict type has changed to something else", Set.empty, Map.empty)


    -- Values backed by JS Kernel types we cannot encode/decode
    ("elm", "virtual-dom", "VirtualDom", "Node")      -> kernelError
    ("elm", "virtual-dom", "VirtualDom", "Attribute") -> kernelError
    ("elm", "virtual-dom", "VirtualDom", "Handler")   -> kernelError
    ("elm", "core", "Process", "Id")                  -> kernelError
    ("elm", "core", "Platform", "ProcessId")          -> kernelError
    ("elm", "core", "Platform", "Program")            -> kernelError
    ("elm", "core", "Platform", "Router")             -> kernelError
    ("elm", "core", "Platform", "Task")               -> kernelError
    ("elm", "core", "Task", "Task")                   -> kernelError
    ("elm", "core", "Platform.Cmd", "Cmd")            -> kernelError
    ("elm", "core", "Platform.Sub", "Sub")            -> kernelError
    ("elm", "json", "Json.Decode", "Decoder")         -> kernelError
    ("elm", "json", "Json.Decode", "Value")           -> kernelError
    ("elm", "json", "Json.Encode", "Value")           -> kernelError
    ("elm", "http", "Http", "Body")                   -> kernelError
    ("elm", "http", "Http", "Part")                   -> kernelError
    ("elm", "http", "Http", "Expect")                 -> kernelError
    ("elm", "http", "Http", "Resolver")               -> kernelError
    ("elm", "parser", "Parser", "Parser")             -> kernelError
    ("elm", "parser", "Parser.Advanced", "Parser")    -> kernelError
    ("elm", "regex", "Regex", "Regex")                -> kernelError

    -- Not Kernel, but have functions... should we have them here?
    -- @TODO remove once we add test for functions in custom types
    ("elm", "url", "Url.Parser", "Parser") -> kernelError
    ("elm", "url", "Url.Parser.Internal", "QueryParser") -> kernelError

    -- Kernel concessions for Frontend Model and Msg
    -- ("elm", "file", "File", "File") -> ("File.File", Set.singleton moduleName, Map.empty)

    -- @TODO improve; These aliases will show up as VirtualDom errors which might confuse users
    -- ("elm", "svg", "Svg", "Svg") -> kernelError
    -- ("elm", "svg", "Svg", "Attribute") -> kernelError

    -- ((ModuleName.Canonical (Pkg.Name "elm" _) (N.Name n)), _) ->
    --   DError $ "❗️unhandled elm type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)
    --
    -- ((ModuleName.Canonical (Pkg.Name "elm-explorations" _) (N.Name n)), _) ->
    --   DError $ "❗️unhandled elm-explorations type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)

    (author, pkg, module_, typeName) ->
      -- Anything else must not be a core type, recurse to find it

      let handleUnion union =
            if isUserDefinedType_ tipe
                then
                  unionToFt oldVersion newVersion scope identifier name interfaces newRecursionSet tvarMap union params
                    & (\(n, imports, subft) ->
                      ( n
                      , if moduleName /= scope then
                          imports & Set.insert moduleName
                        else
                          imports
                      , subft
                          & addImports scope imports
                          & if moduleName /= scope then
                              addImport scope (moduleName) -- <> "(utop)")
                            else
                              id
                      )
                    )
                else
                  ( "old55" -- <> "<!5>"
                      , if moduleName /= scope then
                          Set.empty & Set.insert moduleName
                        else
                          Set.empty
                      , Map.empty
                          & addImports scope Set.empty
                          & if moduleName /= scope then
                              addImport scope (moduleName) -- <> "(utop)")
                            else
                              id
                      )

          handleAlias alias =
            aliasToFt oldVersion newVersion scope identifier name interfaces newRecursionSet alias
              & (\(n, imports, subft) ->
                ( n -- <> "<!4>"
                , if moduleName /= scope then
                    imports & Set.insert moduleName
                  else
                    imports
                , subft
                    & addImports scope imports
                    & if moduleName /= scope then
                        addImport scope (moduleName) -- <> "(utop)")
                      else
                        id
                )
              )
      in
      case findDef (moduleNameRaw) name interfaces of
        Just (Alias alias) -> handleAlias alias
        Just (Union union) -> handleUnion union
        Nothing -> ("XXXXXX subi fail: " <> nameToText moduleNameRaw <> " in " <> (T.pack . show $ (Map.keys interfaces)), Set.empty, Map.empty)



handleSeenRecursiveType :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> Migration
handleSeenRecursiveType oldVersion newVersion scope interfaces recursionSet tipe@(Can.TType moduleName name params) tipeOldM tvarMap =
  -- error "tbc"
  let
      usageMigration :: [Migration]
      usageMigration =
        -- tvarResolvedParams
        params
          & fmap (\param -> canonicalToFt oldVersion newVersion scope interfaces recursionSet param nothingTODO tvarMap)

      usageParams :: Text
      usageParams =
        usageMigration
          & fmap selectMigrationText
          & T.intercalate " "

      usageImports :: Set.Set ModuleName.Canonical
      usageImports =
        usageMigration
          & fmap selectImports
          & mergeAllImports
          -- & Set.insert oldModuleNameCanonical

      usageFts :: MigrationDefinitions
      usageFts =
        usageMigration
          & fmap selectMigrationDefinitions
          & mergeAllMigrationDefinitions

      typeScope :: Text
      typeScope =
        if moduleName == scope then
          ""
        else
          nameToText (canModuleName moduleName) <> "."

      typeName :: Text
      typeName =
        N.toText name
    in
    ( if length params > 0 then
        "(" <> typeScope <> typeName <> " " <> usageParams <> ")" -- <> "<!R>"
      else
        typeScope <> typeName -- <> "<!R>"
    , Set.insert moduleName usageImports
    , usageFts
    )