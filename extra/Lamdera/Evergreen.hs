{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen where


import qualified AST.Canonical as Can
import AST.Module.Name (Canonical(..))
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type


import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Char
import qualified Data.Map as Map
import Data.Map.Strict (unionWithKey)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.List.Index (imap)
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Elm.Interface as Interface
-- import qualified Reporting.Progress as Progress
-- import qualified Stuff.Paths as Paths

import qualified Data.ByteString.Char8 as BS8

-- import qualified File.IO as File
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))
import CanSer.CanSer as CanSer

import qualified AST.Valid as Valid
import qualified Elm.Compiler.Module as Module
import qualified Reporting.Result as Result

import qualified Reporting.Error.LamderaError as LamderaError
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error







import System.FilePath ((</>))
import System.Process (readProcess)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map

import Lamdera
import Lamdera.Types


type SnapRes = (Text, ElmImports, ElmFilesText)

data ElmFileText =
  ElmFileText
    { imports :: [Text]
    , types :: [Text]
    }
  deriving (Show)

type ElmFilesText = Map Text ElmFileText

-- Map <import name> <package>
-- @TODO in future we can use this to pin package versions and adjust import routing to those snapshots
type ElmImports = Map Text Text

selNames (a,b,c) = a
selImports (a,b,c) = b
selFts (a,b,c) = c

getValues m =
  m & Map.toList & fmap snd

lamderaTypes :: [Text]
lamderaTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


snapshotCurrentTypes :: Pkg.Name -> Valid.Module -> Interface.Interfaces -> Result.Result i w Error.Error ()
snapshotCurrentTypes pkg module_@(Valid.Module name _ _ _ _ _ _ _ _ _) interfaces = do

  let !inDebug = unsafePerformIO Lamdera.isDebug

  -- This check isn't strictly required, as the callee of this function in compile only
  -- calls it when we know we've canonicalized the src/Types.elm file, but leaving it here
  -- to prevent any footguns in future testing
  onlyWhen (pkg == (Pkg.Name "author" "project") && name == N.fromText "Types") $ do
    let
      interfaceTypes_elm =
        case Map.lookup (Module.Canonical (Pkg.Name "author" "project") "Types") interfaces of
          Just i -> i
          Nothing -> error "The impossible happened, could not find src/Types.elm"

      efts =
        lamderaTypes
          & fmap (\t -> (t, ftByName interfaces t name interfaceTypes_elm))
          -- & (\v -> unsafePerformIO $ do
          --     formatHaskellValue "ltypes-intermediary" (v) :: IO ()
          --     pure v
          --   )
          & foldl (\acc (t, ft) -> mergeFts acc ft) Map.empty

      debugEfts =
        efts
          & Map.filterWithKey (\k eft -> k == "Types")
          & eftToText

      !_ = unsafePerformIO $ do
        -- formatHaskellValue "some sensible label" (efts) :: IO ()
        putStrLn $ T.unpack $ debugEfts

        root <- getProjectRoot
        efts
          & Map.toList
          & mapM (\(file, ef@(ElmFileText imports types)) ->
            let
              output = efToText ("Evergreen.Types.V1." <> file, ef)

              filename =
                file
                  & T.splitOn "."
                  & foldl (\acc i -> acc </> T.unpack i) (root </> "src" </> "Evergreen" </> "Types" </> "V1")
                  & (\v -> v <> ".elm")

            in
            onlyWhen (not $ textContains "/" file) $ do
              -- putStrLn $ show $ "writing: " <> filename
              writeUtf8 filename output
          )

    Result.ok ()


mergeElmFileText :: Text -> ElmFileText -> ElmFileText -> ElmFileText
mergeElmFileText k ft1 ft2 =
  ElmFileText
    { imports = imports ft1 <> imports ft2 & List.nub
    , types = types ft1 <> types ft2 & List.nub
    }

mergeFts :: ElmFilesText -> ElmFilesText -> ElmFilesText
mergeFts ft1 ft2 =
  unionWithKey mergeElmFileText ft1 ft2


mergeImports :: ElmImports -> ElmImports -> ElmImports
mergeImports i1 i2 =
  unionWithKey (\k v1 v2 -> v1) i1 i2


mergeAllImports :: [ElmImports] -> ElmImports
mergeAllImports imps =
  imps
    & foldl (\acc elmImport -> mergeImports acc elmImport) Map.empty


addImports :: ModuleName.Canonical -> ElmImports -> ElmFilesText -> ElmFilesText
addImports scope@(ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)) imports ft =
  imports
    & foldl (\ft imp -> addImport scope imp ft) ft


addImport :: ModuleName.Canonical -> Text -> ElmFilesText -> ElmFilesText
addImport target@(ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)) imp ft =
  ft
    & Map.alter (\mft ->
      case mft of
        Just ft ->
          Just $ ft { imports = imports ft <> [imp] }

        Nothing ->
          Just $
            ElmFileText
              { imports = [imp]
              , types = []
              }
    ) module_


eftToText :: ElmFilesText -> Text
eftToText ft =
  ft
    & Map.toList
    & List.map (\(file, ef@(ElmFileText imports types)) ->
      "\n\n---------------------------------------------------------------------------------\n" <>
      efToText (file, ef)
    )
    & T.concat


efToText :: (Text, ElmFileText) -> Text
efToText ft =
  case ft of
    (file, ef@(ElmFileText imports types)) ->
      [ "module " <> file <> " exposing (..)\n\n"
      , if length imports > 0 then
          imports
            & List.sort
            & fmap (\imp -> "import " <> imp)
            & T.intercalate "\n"
            & (flip T.append) "\n\n\n"
        else
          ""
      , T.intercalate "\n\n\n" types
      ]
      & T.concat


ftByName :: (Map.Map Canonical Module.Interface) -> Text -> N.Name -> Interface.Interface -> ElmFilesText
ftByName interfaces typeName name interface = do
  let
    scope =
      (ModuleName.Canonical (Pkg.Name "author" "project") (N.Name "Types"))

    recursionIdentifier =
      (scope, N.Name typeName)

    identifier =
      case recursionIdentifier of
        ((ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)), N.Name tipe) ->
          (author, pkg, module_, tipe)

  case Map.lookup (N.fromText typeName) $ Interface._aliases interface of
    Just alias -> do
      let
        diffableAlias = aliasToFt scope identifier typeName interfaces [recursionIdentifier] alias

        -- !x = formatHaskellValue "ftByName.Alias" diffableAlias :: IO ()

        (subt, imps, subft) = diffableAlias

      subft

    Nothing ->
      -- Try unions
      case Map.lookup (N.fromText typeName) $ Interface._unions interface of
        Just union -> do
          let
            diffableUnion = unionToFt scope identifier typeName interfaces [recursionIdentifier] [] union []

            -- !y = formatHaskellValue "ftByName.Union" diffableUnion :: IO ()

            (subt, imps, subft) = diffableUnion

          subft

        Nothing ->
          Map.empty
          -- DError $ "Found no type named " <> typeName <> " in " <> N.toText name


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToFt :: ModuleName.Canonical -> (Text, Text, Text, Text) -> Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> SnapRes
unionToFt scope identifier@(author, pkg, module_, tipe) typeName interfaces recursionMap tvarMap unionInterface params =
  let
    treat union =
      let
        tvarMap = zip (Can._u_vars union) params

        tvars =
          tvarMap
            & fmap (N.toText . fst)
            & T.intercalate " "

        usageParams =
          params
            & fmap (\param -> canonicalToFt scope interfaces recursionMap param tvarMap)
            & fmap selNames
            & T.intercalate " "

        constructors =
          Can._u_alts union
            & fmap (\(Can.Ctor name index int params_) ->
              let
                cparams =
                  -- For each constructor type param
                  fmap (\param -> canonicalToFt scope interfaces recursionMap param tvarMap) params_
              in
              ( N.toText name <> " " <> (fmap (\(t,imps,ft) -> t) cparams & T.intercalate " ")
              , foldl (\acc (st, imps, ft) -> mergeImports acc imps) Map.empty cparams
              , foldl (\acc (st, imps, ft) -> mergeFts acc ft) Map.empty cparams
              )
            )

        ctypes =
          constructors
            & fmap (\(t,imps,ft) -> t)
            & T.intercalate "\n    | "

        imports =
          constructors
            & foldl (\acc (st, imps, ft) -> mergeImports acc imps) Map.empty

        fts =
          constructors
            & foldl (\acc (st, imps, ft) -> mergeFts acc ft) Map.empty

        importScope =
          if moduleName == scope then
            ""
          else
            module_ <> "."

        moduleName =
          (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_))

      in
      ( if length params > 0 then
          "(" <> importScope <> typeName <> " " <> tvars <> ")"
        else
          importScope <> typeName
      , imports
      , (Map.singleton (moduleKey identifier) $
          ElmFileText
            { imports = imports & getValues
            , types =
                if length tvarMap > 0 then
                  ["type " <> typeName <> " " <> tvars <> "\n    = " <> ctypes]
                else
                  ["type " <> typeName <> "\n    = " <> ctypes]
            })
          & mergeFts fts
      )

  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


-- A top level Alias definition i.e. `type alias ...`
aliasToFt :: ModuleName.Canonical -> (Text, Text, Text, Text) -> Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> Interface.Alias -> SnapRes
aliasToFt scope identifier@(author, pkg, module_, tipe) typeName interfaces recursionMap aliasInterface =
  let
    treat a =
      case a of
        Can.Alias tvars tipe ->
          -- let
          --   !_ = formatHaskellValue "aliasToFt" tvars :: IO ()
          -- in
          let
            (subt, imps, subft) = canonicalToFt scope interfaces recursionMap tipe []

            tvars_ =
              tvars
                & fmap N.toText
                & T.intercalate " "

            importScope =
              if moduleName == scope then
                ""
              else
                module_ <> "."

            moduleName =
              (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_))
          in
          ( if length tvars > 0 then
               importScope <> typeName <> " <2>" <> tvars_
             else
               importScope <> typeName
          , Map.empty
          , (Map.singleton (moduleKey identifier) $
              ElmFileText
                { imports = getValues imps
                , types =
                    if length tvars > 0 then
                      ["type alias " <> typeName <> " " <> tvars_ <> " =" <> subt]
                    else
                      ["type alias " <> typeName <> " =" <> subt]
                })
              & mergeFts subft
          )

  in
  case aliasInterface of
    Interface.PublicAlias a -> treat a
    Interface.PrivateAlias a -> treat a


moduleKey :: (Text, Text, Text, Text) -> Text
moduleKey identifier@(author, pkg, module_, tipe) =
  if author == "author" then
    -- Internal package, keep as is
    module_
  else
    -- External package
    author <> "/" <> pkg <> ":" <> module_


moduleNameKey :: ModuleName.Canonical -> Text
moduleNameKey moduleName =
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)) ->
      if author == "author" then
        -- Internal package, keep as is
        module_
      else
        -- External package
        author <> "/" <> pkg <> ":" <> module_


canonicalToFt :: ModuleName.Canonical -> Interface.Interfaces -> [(ModuleName.Canonical, N.Name)] -> Can.Type -> [(N.Name, Can.Type)] -> SnapRes
canonicalToFt scope interfaces recursionMap canonical tvarMap =
  let
    ft = Map.empty

    scopeModule =
      case scope of
        (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)) -> module_

  in
  -- debug $
  case canonical of
    Can.TType moduleName name params ->
      let
        recursionIdentifier = (moduleName, name)

        newRecursionMap = [recursionIdentifier] ++ recursionMap

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

        identifier =
          case (moduleName, name) of
            ((ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)), N.Name tipe) ->
              (author, pkg, module_, tipe)

        kernelError =
          case identifier of
            (author, pkg, module_, tipe) ->
              ("XXXXXX Kernel error", Map.empty, ft)
              -- DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_
      in

      if (List.any ((==) recursionIdentifier) recursionMap) then
        (N.toText name, Map.empty, ft)
        -- DRecursion $ case (moduleName, name) of
        --   ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) (N.Name module_)), N.Name typename) ->
        --     pkg1 <> "/" <> pkg2 <> ":" <> module_ <> "." <> typename

      else
      case identifier of
        ("elm", "core", "String", "String") ->
          ("String", Map.empty, ft)
          -- DString

        ("elm", "core", "Basics", "Int") ->
          ("Int", Map.empty, ft)
          -- DInt

        ("elm", "core", "Basics", "Float") ->
          ("Float", Map.empty, ft)
          -- DFloat

        ("elm", "core", "Basics", "Bool") ->
          ("Bool", Map.empty, ft)
          -- DBool

        ("elm", "core", "Basics", "Order") ->
          ("Order", Map.empty, ft)
          -- DOrder

        ("elm", "core", "Basics", "Never") ->
          ("Never", Map.empty, ft)
          -- DNever

        ("elm", "core", "Char", "Char") ->
          ("Char", Map.empty, ft)
          -- DChar

        ("elm", "core", "Maybe", "Maybe") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("(Maybe " <> subt <> ")", imps, subft)
              -- DMaybe (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Maybe! Please report this."

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("(List " <> subt <> ")", imps, subft)
              -- DList (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossible multi-param List! Please report this."

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("(Array " <> subt <> ")", imps & Map.insert "Array" "Array", subft)
              -- DArray (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Array! Please report this."

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("(Set " <> subt <> ")", imps & Map.insert "Set" "Set", subft)
              -- DSet (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossible multi-param Set! Please report this."

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap result tvarMap)
                (subt2, imps2, subft2) = (canonicalToFt scope interfaces recursionMap err tvarMap)
              in
              ("(Result " <> subt <> " " <> subt2 <> ")", mergeImports imps imps2, mergeFts subft subft2)
              -- DResult (canonicalToFt scope interfaces recursionMap result tvarMap) (canonicalToFt scope interfaces recursionMap err tvarMap)
            _ ->
              error "Fatal: impossible !2 param Result type! Please report this."


        ("elm", "core", "Dict", "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap result tvarMap)
                (subt2, imps2, subft2) = (canonicalToFt scope interfaces recursionMap err tvarMap)
              in
              ("(Dict " <> subt <> " " <> subt2 <> ")", mergeImports imps imps2 & Map.insert "Dict" "Dict", mergeFts subft subft2)
              -- DDict (canonicalToFt scope interfaces recursionMap result tvarMap) (canonicalToFt scope interfaces recursionMap err tvarMap)
            _ ->
              error "Fatal: impossible !2 param Dict type! Please report this."


        -- Values backed by JS Kernel types we cannot encode/decode

        ("elm", "virtual-dom", "VirtualDom", "Node") -> kernelError
        ("elm", "virtual-dom", "VirtualDom", "Attribute") -> kernelError
        ("elm", "virtual-dom", "VirtualDom", "Handler") -> kernelError

        ("elm", "file", "File", "File") -> kernelError

        ("elm", "core", "Process", "Id") -> kernelError
        ("elm", "core", "Platform", "ProcessId") -> kernelError
        ("elm", "core", "Platform", "Program") -> kernelError
        ("elm", "core", "Platform", "Router") -> kernelError
        ("elm", "core", "Platform", "Task") -> kernelError
        ("elm", "core", "Task", "Task") -> kernelError
        ("elm", "core", "Platform.Cmd", "Cmd") -> kernelError
        ("elm", "core", "Platform.Sub", "Sub") -> kernelError

        ("elm", "json", "Json.Decode", "Decoder") -> kernelError
        ("elm", "json", "Json.Decode", "Value") -> kernelError
        ("elm", "json", "Json.Encode", "Value") -> kernelError

        ("elm", "http", "Http", "Body") -> kernelError
        ("elm", "http", "Http", "Part") -> kernelError
        ("elm", "http", "Http", "Expect") -> kernelError
        ("elm", "http", "Http", "Resolver") -> kernelError

        ("elm", "parser", "Parser", "Parser") -> kernelError
        ("elm", "parser", "Parser.Advanced", "Parser") -> kernelError

        ("elm", "regex", "Regex", "Regex") -> kernelError

        -- Not Kernel, but have functions... should we have them here?
        -- @TODO remove once we add test for functions in custom types
        ("elm", "url", "Url.Parser", "Parser") -> kernelError
        ("elm", "url", "Url.Parser.Internal", "QueryParser") -> kernelError

        -- @TODO improve; These aliases will show up as VirtualDom errors which might confuse users
        -- ("elm", "svg", "Svg", "Svg") -> kernelError
        -- ("elm", "svg", "Svg", "Attribute") -> kernelError


        -- ((ModuleName.Canonical (Pkg.Name "elm" _) (N.Name n)), _) ->
        --   DError $ "❗️unhandled elm type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm-explorations" _) (N.Name n)), _) ->
        --   DError $ "❗️unhandled elm-explorations type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)

        (author, pkg, module_, tipe) ->
          -- Anything else must not be a core type, recurse to find it

          case Map.lookup moduleName interfaces of
            Just subInterface ->

              -- Try unions
              case Map.lookup name $ Interface._unions subInterface of
                Just union -> do
                  unionToFt moduleName (author, pkg, module_, tipe) (N.toText name) interfaces newRecursionMap tvarMap union tvarResolvedParams
                    & (\(n, imports, subft) ->
                      ( n
                      , Map.empty
                      , subft
                          & addImports scope imports
                          & if moduleName /= scope then
                              addImport scope (module_) -- <> "(utop)")
                            else
                              id
                      )
                    )

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToFt moduleName (author, pkg, module_, tipe) (N.toText name) interfaces newRecursionMap alias
                        & (\(n, imports, subft) ->
                          ( n
                          , Map.empty
                          , subft
                              & addImports scope imports
                              & if moduleName /= scope then
                                  addImport scope (module_) -- <> "(utop)")
                                else
                                  id
                          )
                        )

                    Nothing ->
                      ("XXXXXX alias lookup fail", Map.empty, ft)
                      -- DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_ <> ". Please report this issue with your code!"

            Nothing ->
              ("XXXXXX subi fail", Map.empty, ft)
              -- let !_ = formatHaskellValue "interface modulenames" (Map.keys interfaces) :: IO ()
              -- in
              -- DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


    Can.TAlias moduleName name tvarMap_ aliasType ->
      let
        module_ =
          case moduleName of
            (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)) -> module_
      in
      case aliasType of
        Can.Holey cType ->
          let

            -- resolveTvars ps =
            --   ps
            --     & fmap (\p ->
            --       case p of
            --         Can.TVar a ->
            --           case List.find (\(t,ti) -> t == a) tvarMap of
            --             Just (_,ti) -> ti
            --             Nothing -> p
            --         _ -> p
            --     )

            usageParamFts =
              tvarMap_
                & fmap (\(n, paramType) ->
                  canonicalToFt moduleName interfaces recursionMap paramType tvarMap_
                )

            usageParamNames =
              usageParamFts
                & fmap selNames
                & T.intercalate " "

            usageParamImports =
              usageParamFts
                & fmap selImports
                & mergeAllImports

                -- & fmap (N.toText . fst)
                -- & T.intercalate " "

            tvars =
              tvarMap_
                & fmap (N.toText . fst)
                & T.intercalate " "

            (subt, imps, subft) = canonicalToFt moduleName interfaces recursionMap cType tvarMap_


            importScope =
              if moduleName == scope then
                ""
              else
                module_ <> "."

            -- moduleName =
            --   (ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_))

            -- !_ = formatHaskellValue "Can.TAlias.Holey" (cType, tvarMap_) :: IO ()
          in
          ( if length tvarMap_ > 0 then
              "(" <> importScope <> N.toText name <> " " <> tvars <> ")"
            else
              importScope <> N.toText name
          , Map.empty -- Map.singleton "Huh?" "What should go here? 2"
          , (Map.singleton (moduleNameKey moduleName) $
              ElmFileText
                { imports = getValues imps
                , types =
                    if length tvarMap_ > 0 then
                      ["type alias " <> N.toText name <> " " <> tvars <> " = " <> subt]
                    else
                      ["type alias " <> N.toText name <> " = " <> subt]
                })
              & mergeFts subft
          )

        Can.Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
          let
            (subt, imps, subft) = canonicalToFt moduleName interfaces recursionMap cType []
          in
          -- (subt, imps, subft)

          ( if module_ == scopeModule then
              N.toText name
            else
              module_ <> "." <> N.toText name
          , Map.empty -- Map.singleton "Huh?" "What should go here? 3"
          , (Map.singleton (moduleNameKey moduleName) $
              ElmFileText
                { imports = getValues imps
                , types = ["type alias " <> N.toText name <> " = " <> subt]
                })
              & mergeFts subft
          )


    Can.TRecord fieldMap isPartial ->
      case isPartial of
        Just whatIsThis ->
          ("XXXXXX TRecord", Map.empty, ft)
          -- DError "must not contain partial records"

        Nothing ->
          let
            -- !_ = formatHaskellValue "Can.TRecord" (fieldMap, tvarMap) :: IO ()

            fields =
              fieldMap
                & Map.toList
                -- Restore user's field code-ordering to keep types looking familiar
                & List.sortOn (\(name, (Can.FieldType index tipe)) -> index)
                & fmap (\(name, (Can.FieldType index tipe)) ->
                  (N.toText name, canonicalToFt scope interfaces recursionMap tipe tvarMap)
                )
                -- & DRecord

            fieldsFormatted =
              fields
                & fmap (\(fieldname, (st, imps, ft)) -> fieldname <> " : " <> st)
                & T.intercalate "\n    , "

            imports =
              fields
                & foldl (\acc (name, (st, imps, ft)) -> mergeImports acc imps) Map.empty

            mergedFt =
              fields
                -- & foldl (\acc (name, (st, imps, ft)) -> mergeImports acc imps) Map.empty
                & foldl (\acc (name, (st, imps, ft)) -> mergeFts acc ft) Map.empty
                & addImports scope imports

          in
          ("\n    { " <> fieldsFormatted <> "\n    }"
          , imports
          , mergedFt
          )

    Can.TTuple firstType secondType maybeType_whatisthisfor ->
      let
        (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap firstType tvarMap)
        (subt2, imps2, subft2) = (canonicalToFt scope interfaces recursionMap secondType tvarMap)
      in
      ("(" <> subt <> ", " <> subt2 <> ")", mergeImports imps imps2, mergeFts subft subft2)
      -- DTuple (canonicalToFt scope interfaces recursionMap firstType tvarMap) (canonicalToFt scope interfaces recursionMap secondType tvarMap)

    Can.TUnit ->
      ("()", Map.empty, ft)

    Can.TVar name ->
      (N.toText name, Map.empty, ft)

    Can.TLambda _ _ ->
      error "Fatal: impossible multi-param Maybe! Please report this."
      ("XXXXXX TLambda", Map.empty, ft)
      -- DError $ "must not contain functions"
