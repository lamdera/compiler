{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen.MigrationGenX where

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
import StandaloneInstances


dothewholething :: Int -> Int -> Interfaces -> Interface.Interface -> IO Text
dothewholething oldVersion newVersion interfaces iface_Types = do
  let
    moduleName = (ModuleName.Canonical (Pkg.Name "author" "project") (N.fromChars $ "Evergreen.V" <> show newVersion <> ".Types"))

    efts =
      lamderaTypes
        & fmap (\t -> (t, ftByName oldVersion newVersion interfaces moduleName t iface_Types))
        & foldl (\acc (t, ft) -> mergeFts acc ft) Map.empty
        -- a little weird but ensures current version of types, our entry point, is added to final migration imports...
        & addImport moduleName moduleName

    -- debugEfts = efts & eftToText version

    importsToText :: Set.Set ModuleName.Canonical -> [Text]
    importsToText imports =
      imports
            & Set.toList
            & filterMap (\(ModuleName.Canonical (Pkg.Name author project) module_) ->
              Just $ "import " <> nameToText module_
            )
            & List.sort

    addMigrations migrations = migrations & List.sort & T.intercalate "\n\n"

  inDebug <- Lamdera.isDebug
  root <- getProjectRoot

  let
    allMigrations :: Text
    allMigrations =
        efts
          & Map.toList
          & fmap (\(file, ef@(ElmFileText imports migrations)) -> migrations )
          & List.concat
          & List.sort
          & T.intercalate "\n\n"

    allImports :: Text
    allImports =
        efts
          & Map.toList
          & fmap (\(file, ef@(ElmFileText imports migrations)) -> importsToText imports )
          & List.concat
          & List.sort
          & T.intercalate "\n"

  pure $ ("module Evergreen.Migrate.V" <> show_ newVersion <> " exposing (..)\n\n")
    <> allImports
    <> "\n\n"
    <> allMigrations


type RecursionSet =
  -- [(ModuleName.Raw, N.Name)]
  Set.Set (ModuleName.Canonical, N.Name)

-- type TypeIdentifier = (Text, Text, Text, Text)
type TypeIdentifier = (Pkg.Author, Pkg.Project, N.Name, N.Name)

type SnapRes = (Text, ElmImports, ElmFilesText)

data ElmFileText =
  ElmFileText
    { imports :: ElmImports
    , types :: [Text]
    }
  deriving (Show, Eq)

type ElmFilesText = Map Text ElmFileText

-- Map <import name> <package>
-- @TODO in future we can use this to pin package versions and adjust import routing to those snapshots
type ElmImports = Set.Set ModuleName.Canonical

selectNames (a,b,c) = a
selectImports (a,b,c) = b
selectFts (a,b,c) = c

lamderaTypes :: [N.Name]
lamderaTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


mergeElmFileText :: Text -> ElmFileText -> ElmFileText -> ElmFileText
mergeElmFileText k ft1 ft2 =
  ElmFileText
    { imports = Set.union (imports ft1) (imports ft2)
    , types = (types ft1 <> types ft2) & List.nub
    }

mergeFts :: ElmFilesText -> ElmFilesText -> ElmFilesText
mergeFts ft1 ft2 = unionWithKey mergeElmFileText ft1 ft2

mergeAllFts :: [ElmFilesText] -> ElmFilesText
mergeAllFts ftss = ftss & foldl (\acc fts -> mergeFts acc fts) Map.empty

mergeImports :: ElmImports -> ElmImports -> ElmImports
mergeImports i1 i2 = Set.union i1 i2

mergeAllImports :: [ElmImports] -> ElmImports
mergeAllImports imps = imps & foldl (\acc elmImport -> mergeImports acc elmImport) Set.empty

addImports :: ModuleName.Canonical -> ElmImports -> ElmFilesText -> ElmFilesText
addImports scope@(ModuleName.Canonical (Pkg.Name author pkg) module_) imports ft =
  imports & foldl (\ft imp -> addImport scope imp ft) ft

addImport :: ModuleName.Canonical -> ModuleName.Canonical -> ElmFilesText -> ElmFilesText
addImport moduleName imp ft =
  ft
    & Map.alter (\mft ->
      case mft of
        Just ft ->
          Just $ ft { imports = imports ft & Set.insert imp }

        Nothing ->
          Just $
            ElmFileText
              { imports = Set.singleton imp
              , types = []
              }
    ) (moduleNameKey moduleName)


-- eftToText :: Int -> ElmFilesText -> Text
-- eftToText version ft =
--   ft
--     & Map.toList
--     & List.map (\(file, ef@(ElmFileText imports types)) ->
--       "\n\n---------------------------------------------------------------------------------\n" <>
--       efToText version (file, ef)
--     )
--     & T.concat


-- efToText :: Int -> (Text, ElmFileText) -> Text
-- efToText version ft =
--   case ft of
--     (file, ef@(ElmFileText imports types)) ->
--       [ if length imports > 0 then
--           imports
--             & Set.toList
--             & filterMap (\(ModuleName.Canonical (Pkg.Name author project) module_) ->
--                 if (nameToText module_ == file) then
--                   Nothing
--                 else if (author == "author") then
--                   Just $ "import " <> nameToText module_ -- <> " as " <> nameToText module_
--                 else
--                   Just $ "import " <> nameToText module_
--             )
--             & List.sort
--             & T.intercalate "\n"
--             & (flip T.append) "\n\n\n"
--         else
--           ""
--       , T.intercalate "\n\n\n" types
--       ]
--       & T.concat


ftByName :: Int -> Int -> Interfaces -> ModuleName.Canonical -> N.Name -> Interface.Interface -> ElmFilesText
ftByName oldVersion newVersion interfaces moduleName typeName interface = do
  let
    scope = moduleName
    recursionIdentifier = (scope, typeName)
    recursionSet = Set.singleton recursionIdentifier
    identifier = asIdentifier_ recursionIdentifier

  case Map.lookup typeName $ Interface._aliases interface of
    Just alias -> do
      let
        diffableAlias = aliasToFt oldVersion newVersion scope identifier typeName interfaces recursionSet alias
        (subt, imps, subft) = diffableAlias

      subft & addImports moduleName imps

    Nothing ->
      -- Try unions
      case Map.lookup typeName $ Interface._unions interface of
        Just union -> do
          let
            diffableUnion = unionToFt oldVersion newVersion scope identifier typeName interfaces recursionSet [] union []
            (subt, imps, subft) = diffableUnion

          subft & addImports moduleName imps

        Nothing ->
          -- DError $ "Found no type named " <> nameToText typeName <> " in " <> N.toText name
          Map.empty


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToFt :: Int -> Int -> ModuleName.Canonical -> TypeIdentifier -> N.Name -> Interfaces -> RecursionSet -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> SnapRes
unionToFt oldVersion newVersion scope identifier@(author, pkg, newModule, tipe) typeName interfaces recursionSet tvarMap unionInterface params =
  let
    oldModuleName =
      newModule
        & N.toText
        & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ("Evergreen.V" <> show_ oldVersion <> ".")
        & N.fromText

    oldModuleNameCanonical =
      (ModuleName.Canonical (Pkg.Name "author" "project") oldModuleName)

    tipeOld =
      case Map.lookup oldModuleName interfaces of
        Just oldModuleInterface ->
          case Map.lookup typeName $ Interface._aliases oldModuleInterface of
            Just aliasInterface ->
                case aliasInterface of
                  Interface.PublicAlias a -> Just $ Alias a
                  Interface.PrivateAlias a -> Just $ Alias a

            Nothing ->
              case Map.lookup typeName $ Interface._unions oldModuleInterface of
                Just unionInterface -> do
                  case unionInterface of
                    Interface.OpenUnion u -> Just $ Union u
                    Interface.ClosedUnion u -> Just $ Union u
                    Interface.PrivateUnion u -> Just $ Union u

                Nothing ->
                  -- error $ "could not find old type at all: " <> show (module_, tipe)
                  Nothing

        Nothing ->
          Nothing
          -- error $ "could not find old module at all: " <> show (module_, tipe)
          -- error $ "could not find '" <> N.toChars oldModuleName <> "' old module at all: " <> show (Map.keys interfaces)


    mainLogic =
      case tipeOld of
        Nothing        -> ("Unimplemented -- Cannot find any old type with same name as new type", Set.empty, Map.empty)
        Just (Alias a) -> ("Unimplemented -- Old type was an Custom type, new type is an Alias", Set.empty, Map.empty)
        Just (Union oldUnion) ->
          case unionInterface of
            Interface.OpenUnion newUnion -> migrateUnion oldUnion newUnion
            Interface.ClosedUnion newUnion -> migrateUnion oldUnion newUnion
            Interface.PrivateUnion newUnion -> migrateUnion oldUnion newUnion

    migrateUnion oldUnion newUnion =
      let
        tvarMap =
          zip (Can._u_vars newUnion) params

        tvars_ =
          tvarMap
            & fmap (N.toText . fst)
            & T.intercalate " "

        tvarResolvedParams =
          params
            & fmap (\p ->
              case p of
                Can.TVar a ->
                  case List.find (\(t,ti) -> t == a) tvarMap of
                    Just (_,ti) -> ti
                    Nothing -> p
                _ -> p
            )

        usageSnapRes =
          tvarResolvedParams
            & fmap (\param -> canonicalToFt oldVersion newVersion scope interfaces recursionSet param nothingTODO tvarMap)

        usageParams =
          usageSnapRes
            & fmap selectNames
            & T.intercalate " "

        usageImports =
          usageSnapRes
            & fmap selectImports
            & mergeAllImports
            & Set.insert oldModuleNameCanonical

        usageFts =
          usageSnapRes
            & fmap selectFts
            & mergeAllFts

        localScope =
          (ModuleName.Canonical (Pkg.Name author pkg) newModule)

        oldConstructorFts =
          Can._u_alts oldUnion
            & fmap (\(Can.Ctor oldConstructor index int oldParams) ->
              -- For each OLD constructor type param
              let
                cparams =
                  -- @TODO we need to include new params here as well?
                  oldParams &
                    fmap (\param ->
                      canonicalToFt oldVersion newVersion localScope interfaces recursionSet param nothingTODO tvarMap
                      )

                newCtorM = Can._u_alts newUnion & List.find (\(Can.Ctor newConstructor _ _ _) -> newConstructor == oldConstructor )

              in
              ( case newCtorM of
                  Just (Can.Ctor newConstructor _ _ newParams) ->
                    let
                      paramsZipped =
                        zipFull oldParams newParams
                          & imap (\i (paramOldM, paramNewM) ->
                            case (paramOldM, paramNewM) of
                              (Just paramOld, Just paramNew) ->
                                let ft@(subt, imps, subft) =
                                      canonicalToFt oldVersion newVersion localScope interfaces recursionSet paramOld (Just paramNew) tvarMap
                                in
                                if paramOld == paramNew then
                                  "p" <> show_ i
                                else if isAnonymousRecord paramOld then
                                  subt
                                else if isUserType_ paramOld then
                                  " (migrate" <> asTypeName paramOld <>  " p" <> show_ i <> ")"
                                else
                                  "p" <> show_ i

                              (Just paramOld, Nothing) -> "\n-- warning: old variant didn't get mapped to anything, check this is what you want\n"

                              (Nothing, Just paramNew) -> "(Debug.todo \"this new variant needs to be initialised!\")"

                              _ -> error "impossible, zip produced a value without any contents"

                          )

                      migration =
                        if oldParams == newParams then
                          moduleScope <> N.toText newConstructor <> " " <>
                            (paramsZipped
                              & T.intercalate " "
                            )
                        else
                          "Unimplemented"
                    in
                    if List.length oldParams > 0 then
                      "    " <> N.toText oldModuleName <> "." <> N.toText oldConstructor <> " " <> (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " ") <> " -> " <> migration <> "\n"
                    else
                      "    " <> N.toText oldModuleName <> "." <> N.toText oldConstructor <> " -> " <> moduleScope <> N.toText newConstructor <> "\n"

                  Nothing -> do
                    -- No old constructor with same name, so this is a new/renamed constructor
                    let unimplemented =
                          " ->\n" <>
                          "           {- `" <> N.toText oldConstructor <> "` doesn't exist in " <> moduleScope <> N.toText typeName <> " so I couldn't figure out how to migrate it!\n" <>
                          "           You'll need to decide what happens to " <> N.toText oldModuleName <> "." <> N.toText oldConstructor <> " values in a migration.\n" <>
                          "           See https://lamdera.com/tips/modified-custom-type for more info. -}\n" <>
                          "           Unimplemented\n"
                    if List.length oldParams > 0 then
                      "    " <> N.toText oldModuleName <> "." <> N.toText oldConstructor <> " " <> (imap (\i _ -> "p" <> show_ i) oldParams & T.intercalate " ") <> unimplemented
                    else
                      "    " <> N.toText oldModuleName <> "." <> N.toText oldConstructor <> unimplemented

              , foldl (\acc (st, imps, ft) -> mergeImports acc imps) Set.empty cparams
              , foldl (\acc (st, imps, ft) -> mergeFts acc ft) Map.empty cparams
              )
            )

        newConstructorWarnings =
          Can._u_alts newUnion
            & filterMap (\(Can.Ctor newConstructor index int newParams) -> do
              case Can._u_alts oldUnion & List.find (\(Can.Ctor oldConstructor _ _ _) -> newConstructor == oldConstructor ) of
                Nothing ->
                  -- This constructor is missing a match in the old type, warn the user this new constructor exists
                  Just $
                    "    notices ->\n" <>
                    "        {- `" <> N.toText newConstructor <> "` doesn't exist in " <> moduleScope <> N.toText typeName <> ".\n" <>
                    "        This is just a reminder in case migrating some subset of the old data to this new value was important.\n" <>
                    "        See https://lamdera.com/tips/modified-custom-type for more info. -}\n" <>
                    "        Notice"
                Just _ ->
                  -- This constructor has a match in the old type, so skip it
                  Nothing
            )


        ctypes =
          oldConstructorFts
            & fmap (\(t,imps,ft) -> t)
            -- & T.intercalate " -> todo \n"
            & flip (++) newConstructorWarnings
            & T.concat

        imports = oldConstructorFts & foldl (\acc (st, imps, ft) -> mergeImports acc imps) Set.empty

        fts = oldConstructorFts & foldl (\acc (st, imps, ft) -> mergeFts acc ft) Map.empty

        moduleScope = nameToText newModule <> "."
        typeScope = nameToText newModule <> "."
        typeScopeOld = nameToText newModule <> "."

        moduleName = (ModuleName.Canonical (Pkg.Name author pkg) newModule)

        debug (t, imps, ft) =
          -- debugHaskellWhen (typeName == "RoomId") ("dunion: " <> hindentFormatValue scope) (t, imps, ft)
          debugNote ("\n✴️  inserting def for " <> t) (t, imps, ft)

        migrationNameUnderscored = newModule
          & N.toText
          & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ""
          & T.replace "." "_"

        migration =
          if length tvarMap > 0 then
            "(migrate_" <> migrationNameUnderscored <> "_" <> nameToText typeName <> " " <> usageParams <> ")" -- <> "<!2>"
          else
            "migrate_" <> migrationNameUnderscored <> "_" <> nameToText typeName -- <> "<!3>"

      in
      -- debug $
      ( migration
      , usageImports
      , (Map.singleton (moduleKey identifier) $
          ElmFileText
            { imports = imports
            , types = [ "migrate_" <> migrationNameUnderscored <> "_" <> nameToText typeName <> " old =\n" <>
                        "  case old of\n" <>
                        ctypes
                      ]
            })
          & mergeFts fts
          & mergeFts usageFts
      )

  in
  mainLogic


data OldDef = Alias Can.Alias | Union Can.Union deriving (Show)


-- A top level Alias definition i.e. `type alias ...`
aliasToFt :: Int -> Int -> ModuleName.Canonical -> TypeIdentifier -> N.Name -> Interfaces -> RecursionSet -> Interface.Alias -> SnapRes
aliasToFt oldVersion newVersion scope identifier@(author, pkg, module_, tipe) typeName interfaces recursionSet aliasInterface =
  let

    oldModuleInterface = interfaces Map.! "Evergreen.V1.Types" -- @TODO

    tipeOld =
      case Map.lookup typeName $ Interface._aliases oldModuleInterface of
        Just aliasInterface ->
            case aliasInterface of
              Interface.PublicAlias a -> Alias a
              Interface.PrivateAlias a -> Alias a

        Nothing ->
          case Map.lookup typeName $ Interface._unions oldModuleInterface of
            Just unionInterface -> do
              case unionInterface of
                Interface.OpenUnion u -> Union u
                Interface.ClosedUnion u -> Union u
                Interface.PrivateUnion u -> Union u

            Nothing ->
              error "could not find old type at all @TODO"

    treat a =
      -- let debug (t, imps, ft) = debugNote ("\n🔵  inserting def for " <> t) (t, imps, ft)
      -- in
      -- debug $
      case a of
        Can.Alias tvars tipe ->
          case tipeOld of
            Alias (Can.Alias tvarsOld tipeOld) ->
              let
                (subt, imps, subft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe (Just tipeOld) []

                tvars_ = tvars & fmap N.toText & T.intercalate " "

                typeScope =
                  if moduleName == scope then
                    ""
                  else if isUserType identifier then
                    nameToText module_ <> "."
                  else
                    nameToText module_ <> "."

                moduleName = (ModuleName.Canonical (Pkg.Name author pkg) module_)

                migration =
                  if tipe == tipeOld then
                    "ModelUnchanged"
                  else
                    subt

              in
              ( if length tvars > 0 then
                  "(" <> typeScope <> nameToText typeName <> tvars_ <> ")" -- <> "<!2>"
                else
                  typeScope <> nameToText typeName -- <> "<!3>"
              , imps
              , (Map.singleton (moduleKey identifier) $
                  ElmFileText
                    { imports = imps
                    , types = [
                        "\n" <> (lowerFirstLetter_ $ nameToText typeName) <> " : Old." <> nameToText typeName <> " -> ModelMigration New." <> nameToText typeName <> " New." <> msgForType (nameToText typeName) <> "\n" <>
                        (lowerFirstLetter_ $ nameToText typeName) <> " old = " <> migration
                      ]
                    })
                  & mergeFts subft
              )
            _ -> error "old union became alias"
  in
  case aliasInterface of
    Interface.PublicAlias a -> treat a
    Interface.PrivateAlias a -> treat a


canonicalToFt :: Int -> Int -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> Maybe Can.Type -> [(N.Name, Can.Type)] -> SnapRes
canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe tipeOldM tvarMap =
  let
    scopeModule =
      case scope of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) -> module_

    debug (t, imps, ft) =
      -- debugHaskellWhen (textContains "OptionalData" t) ("\n✳️  inserting def for " <> t <> "\n" <> (T.pack . show $ canonical)) (t, imps, ft)
      -- debug_note ("🔵inserting def for " <> T.unpack t <> ":\n" <> ( ft)) $ (t, imps, ft)
      unsafePerformIO $ do
          formatHaskellValue ("\n🔵inserting def for " <> (show_ tipeOldM)) (t, ft) :: IO ()
          -- formatHaskellValue ("\n🟠 which had oldtype: " <> t) (tipeOldM) :: IO ()
          pure (t, imps, ft)
  in
  debug $
  case tipe of
    Can.TType moduleName name params ->
      let
        recursionIdentifier = (moduleName, name)

        newRecursionSet = Set.insert recursionIdentifier recursionSet

        identifier = asIdentifier tipe

        kernelError =
          case identifier of
            (author, pkg, module_, typeName) ->
              ("XXXXXX Kernel error", Set.empty, Map.empty)
              -- DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> nameToText module_

        moduleNameRaw =
          case moduleName of
            ModuleName.Canonical (Pkg.Name author pkg) module_ ->
              module_
      in

      if (Set.member recursionIdentifier recursionSet) then

        let
          usageSnapRes =
            params
              & fmap (\param -> canonicalToFt oldVersion newVersion scope interfaces recursionSet param Nothing tvarMap)

          usageParams =
            usageSnapRes
              & fmap selectNames
              & T.intercalate " "

          usageImports =
            usageSnapRes
              & fmap selectImports
              & mergeAllImports

          usageFts =
            usageSnapRes
              & fmap selectFts
              & mergeAllFts

          typeScope =
            if moduleName == scope then
              ""
            else if isUserType identifier then
              nameToText module_ <> "."
            else
              nameToText module_ <> "."

          typeName =
            N.toText name

          module_ =
            case moduleName of
              (ModuleName.Canonical (Pkg.Name author pkg) module_) -> module_

        in
        ( if length params > 0 then
            "(" <> typeScope <> typeName <> " " <> usageParams <> ")" -- <> "<!R>"
          else
            typeScope <> typeName -- <> "<!R>"
        , Set.insert moduleName usageImports
        , usageFts
        )

      else
      case identifier of
        ("elm", "core", "String", "String") -> ("String", Set.empty, Map.empty)
        ("elm", "core", "Basics", "Int") -> ("Int", Set.empty, Map.empty)
        ("elm", "core", "Basics", "Float") -> basicUnimplemented tipeOldM tipe
        ("elm", "core", "Basics", "Bool") -> ("Bool", Set.empty, Map.empty)
        ("elm", "core", "Basics", "Order") -> ("Order", Set.empty, Map.empty)
        ("elm", "core", "Basics", "Never") -> ("Never", Set.empty, Map.empty)
        ("elm", "core", "Char", "Char") -> ("Char", Set.empty, Map.empty)

        ("elm", "core", "Maybe", "Maybe") ->
          case params of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p nothingTODO tvarMap)
              in
              ("(Maybe " <> subt <> ")", imps, subft)
              -- DMaybe (canonicalToFt oldVersion newVersion scope interfaces recursionSet p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Maybe! Please report this."

        ("elm", "core", "List", "List") ->
          case params of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p nothingTODO tvarMap)
              in
              ("(List " <> subt <> ")", imps, subft)
              -- DList (canonicalToFt oldVersion newVersion scope interfaces recursionSet p tvarMap)
            _ ->
              error "Fatal: impossible multi-param List! Please report this."

        ("elm", "core", "Array", "Array") ->
          case params of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p nothingTODO tvarMap)
              in
              ("(Array.Array " <> subt <> ")", imps & Set.insert moduleName, subft)
              -- DArray (canonicalToFt oldVersion newVersion scope interfaces recursionSet p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Array! Please report this."

        ("elm", "core", "Set", "Set") ->
          case params of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet p nothingTODO tvarMap)
              in
              ("(Set.Set " <> subt <> ")", imps & Set.insert moduleName, subft)
              -- DSet (canonicalToFt oldVersion newVersion scope interfaces recursionSet p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Set! Please report this."

        ("elm", "core", "Result", "Result") ->
          -- @TODO next: extract result type
          case tipeOldM of
            Just tipeOld ->
              case tipeOld of
                Can.TType _ _ paramsOld ->
                  zipFull paramsOld params & (\p ->
                    case p of
                      (Just resultOld,Just result):(Just errOld,Just err):_ ->
                        let
                          (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet result (Just resultOld) tvarMap)
                          (subt2, imps2, subft2) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet err (Just errOld) tvarMap)
                        in
                        ("(Result " <> subt <> " " <> subt2 <> ")", mergeImports imps imps2, mergeFts subft subft2)
                        -- DResult (canonicalToFt oldVersion newVersion scope interfaces recursionSet result tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet err tvarMap)
                      _ ->
                        error "Fatal: impossible !2 param Result type! Please report this."

                  )

                _ -> error "unexpected type"
            Nothing ->
              ("BLAH", Set.empty, Map.empty)
              -- error "got nothing but shouldn't have"


        ("elm", "core", "Dict", "Dict") ->
          case params of
            result:err:_ ->
              let
                (subt, imps, subft) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet result nothingTODO tvarMap)
                (subt2, imps2, subft2) = (canonicalToFt oldVersion newVersion scope interfaces recursionSet err nothingTODO tvarMap)
              in
              ("(Dict.Dict " <> subt <> " " <> subt2 <> ")", mergeImports imps imps2 & Set.insert moduleName, mergeFts subft subft2)
              -- DDict (canonicalToFt oldVersion newVersion scope interfaces recursionSet result tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet err tvarMap)
            _ ->
              error "Fatal: impossible !2 param Dict type! Please report this."


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
        ("elm", "file", "File", "File") -> ("File.File", Set.singleton moduleName, Map.empty)


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

          case Map.lookup moduleNameRaw interfaces of
            Just subInterface ->

              -- Try unions
              case Map.lookup name $ Interface._unions subInterface of
                Just union -> do
                  if isUserType_ tipe
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

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
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

                    Nothing ->
                      ("XXXXXX alias lookup fail", Set.empty, Map.empty)
                      -- DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> nameToText module_ <> ". Please report this issue with your code!"

            Nothing ->
              ("XXXXXX subi fail: " <> nameToText moduleNameRaw <> " in " <> (T.pack . show $ (Map.keys interfaces)), Set.empty, Map.empty)
              -- let !_ = formatHaskellValue "interface modulenames" (Map.keys interfaces) :: IO ()
              -- in
              -- DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> nameToText module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


    Can.TAlias moduleName name tvarMap_ aliasType ->
      let
        module_ =
          case moduleName of
            (ModuleName.Canonical (Pkg.Name author pkg) module_) -> module_

        identifier = asIdentifier_ (moduleName, name)

      in
      case aliasType of
        Can.Holey cType ->
          let
            usageParamFts =
              tvarMap_
                & fmap (\(n, paramType) ->
                  canonicalToFt oldVersion newVersion scope interfaces recursionSet paramType nothingTODO tvarMap_
                )

            usageParamNames =
              usageParamFts
                & fmap selectNames
                & T.intercalate " "

            usageParamImports =
              usageParamFts
                & fmap selectImports
                & mergeAllImports

            tvars =
              tvarMap_
                & fmap (N.toText . fst)
                & T.intercalate " "

            (subt, imps, subft) =
              case tipeOldM of
                Just tipeOld ->
                  canonicalToFt oldVersion newVersion moduleName interfaces recursionSet cType (Just tipeOld) tvarMap_
                Nothing ->
                  canonicalToFt oldVersion newVersion moduleName interfaces recursionSet cType nothingTODO tvarMap_

            typeScope =
              if moduleName == scope then
                ""
              else if isUserType identifier then
                nameToText module_ <> "."
              else
                nameToText module_ <> "."

            debugIden = "" -- <> "<ah>"

            scopeImports =
              -- if moduleName == scope then
              --   usageParamImports
              -- else
                usageParamImports & Set.insert moduleName

            typeDef =

              if isUserType_ cType then
                if length tvarMap_ > 0 then
                    ["migrate" <> N.toText name <> "4 old = " <> subt]
                else
                    ["migrate" <> N.toText name <> "5 old = " <> subt]
              else
                -- ["-- no migration for primitive: " <> N.toText name ]
                []

            thing =
              (Map.singleton (moduleNameKey moduleName) $
                    ElmFileText
                      { imports = imps
                      , types = typeDef
                      })
                    & mergeFts subft
                    & mergeFts (mergeAllFts (fmap selectFts usageParamFts))

            -- !_ = formatHaskellValue "Can.TAlias.Holey:" (name, cType, tvarMap_, moduleName, scope) :: IO ()
          in
          -- debug_note ("🔵inserting def for " <> T.unpack (moduleNameKey moduleName) <> "." <> N.toString name <> "\n" <> (T.unpack $ head typeDef)) $

          case tipeOldM of
            Just tipeOld ->
              if tipeOld == tipe then
                ("SAMETYPES", scopeImports, thing)
              else
                (
                  debugIden <> "🔵" <>
                  if length tvarMap_ > 0 then
                    "(" <> typeScope <> N.toText name <> " " <> usageParamNames <> ")"
                  else
                    typeScope <> N.toText name
                , scopeImports
                , thing
                )
            _ -> ("-- TODO old type gone for type: " <> show_ tipe, scopeImports, thing)


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
            if module_ == scopeModule then
              N.toText name
            else if isUserType identifier then
              nameToText module_ <> "."
            else
              nameToText module_ <> "." <> N.toText name
          , imps
          , (Map.singleton (moduleNameKey moduleName) $
              ElmFileText
                { imports = imps
                , types = ["1type alias " <> N.toText name <> " = " <> subt]
                })
              & mergeFts subft
          )


    Can.TRecord fieldMap isPartial ->
      case isPartial of
        Just whatIsThis ->
          ("ERROR TRecord, please report this!", Set.empty, Map.empty)
          -- DError "must not contain partial records"

        Nothing ->
          let
            fieldMapOld =
              case tipeOldM of
                Just tipeOld ->
                  case tipeOld of
                    Can.TRecord fieldMapOld isPartialOld ->
                      fieldMapOld
                    -- _ -> error $ "field map old not record: " <> show tipe
                    _ -> Map.empty
                -- _ -> error $ "field map old is empty for TRecord: " <> show tipe
                _ -> Map.empty

            fields =
              fieldMap
                & Map.toList
                -- Restore user's field code-ordering to keep types looking familiar
                & List.sortOn (\(name, (Can.FieldType index tipe)) -> index)
                & fmap (\(name, (Can.FieldType index tipe)) ->
                    case Map.lookup name fieldMapOld of
                      Just (Can.FieldType index_ tipeOld) ->
                        if tipeOld == tipe then
                          (N.toText name, ("old." <> N.toText name, Set.empty, Map.empty))
                        else
                          -- @TODO NEXT
                          -- this shoudl generate a migrate<NewTypeName> function – but maybe that comes from the canonicalToFt?
                          -- at the least we can thread the tipeOld type in here now!
                          let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe (Just (tipeOld)) tvarMap
                          in
                          (N.toText name, ("old." <> N.toText name <> " |> " <> st, imps, ft))
                          -- (N.toText name, canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap)

                      Nothing ->
                        -- This field did not exist in the old version. We need an init! @TODO
                        let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap
                        in
                        ( N.toText name, ("Unimplemented -- new field of type: " <> qualifiedTypeName tipe, imps, ft) )
                )
                & (\v -> v ++ missingFields)

            missingFields =
              fieldMapOld
                & Map.toList
                & filterMap (\(name, (Can.FieldType index tipe)) ->
                  case Map.lookup name fieldMap of
                    Just (Can.FieldType index_ tipeOld) ->
                      Nothing
                    Nothing ->
                      let (st,imps,ft) = canonicalToFt oldVersion newVersion scope interfaces recursionSet tipe nothingTODO tvarMap
                      in
                      Just ( N.toText name, ("Warning -- " <> qualifiedTypeName tipe <> " field removed. either remove this line (data dropped) or migrate into another field.", imps, ft) )
                )

            fieldsFormatted =
              fields
                & fmap (\(fieldname, (st, imps, ft)) -> fieldname <> " = " <> st)
                & T.intercalate "\n    , "

            imports =
              fields
                & foldl (\acc (name, (st, imps, ft)) -> mergeImports acc imps) Set.empty

            mergedFt =
              fields
                & foldl (\acc (name, (st, imps, ft)) -> mergeFts acc ft) Map.empty
                & addImports scope imports

            result =
              (" { " <> fieldsFormatted <> "\n    }"
              , imports
              , mergedFt
              )
          in
          result

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
          ("(" <> subt <> ", " <> subt2 <> ", " <> subt3 <> ")", mergeAllImports [imps,imps2,imps3], mergeAllFts [subft,subft2,subft3])

        Nothing ->
          ("(" <> subt <> ", " <> subt2 <> ")", mergeImports imps imps2, mergeFts subft subft2)
      -- DTuple (canonicalToFt oldVersion newVersion scope interfaces recursionSet t1 tvarMap) (canonicalToFt oldVersion newVersion scope interfaces recursionSet t2 tvarMap)

    Can.TUnit ->
      ("()", Set.empty, Map.empty)

    Can.TVar name ->
      (N.toText name, Set.empty, Map.empty)

    Can.TLambda _ _ ->
      error "Fatal: impossible function type! Please report this."
      -- ("XXXXXX TLambda", Set.empty, Map.empty)
      -- DError $ "must not contain functions"


basicUnimplemented tipeOldM tipe =
  case tipeOldM of
    Just tipeOld ->
      if tipeOld /= tipe then
        ("Unimplemented -- expecting: " <> (qualifiedTypeName tipeOld) <> " -> " <> qualifiedTypeName tipe, Set.empty, Map.empty)
      else
        ("UHHHH1", Set.empty, Map.empty)
    Nothing ->
      ("UHHHH2", Set.empty, Map.empty)



moduleKey :: TypeIdentifier -> Text
moduleKey identifier@(author, pkg, module_, tipe) =
  if utf8ToText author == "author" then
    -- Internal package, keep as is
    nameToText module_
  else
    -- External package
    utf8ToText author <> "/" <> utf8ToText pkg <> ":" <> nameToText module_

idTypeName :: TypeIdentifier -> Text
idTypeName identifier@(author, pkg, module_, tipe) =
  N.toText tipe

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


asIdentifier :: Can.Type -> TypeIdentifier
asIdentifier tipe =
  case tipe of
    Can.TType moduleName name params ->
      asIdentifier_ (moduleName, name)

    _ -> error $ "asIdentifierUnimplemented: " <> show tipe


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
  case asIdentifier tipe of
    (author, pkg, module_, typeName) -> N.toText module_ <> "." <> N.toText typeName



isUserType :: TypeIdentifier -> Bool
isUserType (author, pkg, module_, tipe) =
  author == "author" && pkg == "project"


isUserType_ :: Can.Type -> Bool
isUserType_ cType =
  case cType of
    Can.TType moduleName name params -> isUserModule moduleName
    Can.TLambda _ _ -> True
    Can.TVar _ -> True
    Can.TRecord _ _ -> True
    Can.TUnit -> False
    Can.TTuple _ _ _ -> False
    Can.TAlias _ _ _ aType ->
      case aType of
        Can.Holey t -> isUserType_ t
        Can.Filled t -> isUserType_ t

isAnonymousRecord :: Can.Type -> Bool
isAnonymousRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False


isUserModule moduleName =
    case moduleName of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
            author == "author" && pkg == "project"

migrationWrapperForType t =
  case t of
    "BackendModel"  -> "ModelMigration"
    "FrontendModel" -> "ModelMigration"
    "FrontendMsg"   -> "MsgMigration"
    "ToBackend"     -> "MsgMigration"
    "BackendMsg"    -> "MsgMigration"
    "ToFrontend"    -> "MsgMigration"

msgForType t =
  case t of
    "BackendModel"  -> "BackendMsg"
    "FrontendModel" -> "FrontendMsg"
    "FrontendMsg"   -> "FrontendMsg"
    "ToBackend"     -> "BackendMsg"
    "BackendMsg"    -> "BackendMsg"
    "ToFrontend"    -> "FrontendMsg"

unchangedForType t =
  case t of
    "BackendModel"  -> "ModelUnchanged"
    "FrontendModel" -> "ModelUnchanged"
    "FrontendMsg"   -> "MsgUnchanged"
    "ToBackend"     -> "MsgUnchanged"
    "BackendMsg"    -> "MsgUnchanged"
    "ToFrontend"    -> "MsgUnchanged"


nothingTODO = Nothing
