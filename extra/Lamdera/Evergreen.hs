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


-- data SnapRes =
--   SnapRes
--     { typeText :: Text
--     , ei :: ElmImports
--     , eft :: ElmFilesText
--     }
--   deriving (Show)

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

sel2 (a,b,c) = b
sel3 (a,b,c) = c

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

      typediffs =
        lamderaTypes
          & fmap (\t -> (t, ftByName interfaces t name interfaceTypes_elm))
          & (\v -> unsafePerformIO $ do
              formatHaskellValue "ltypes-intermediary" (v) :: IO ()
              pure v
            )
          & foldl (\acc (t, ft) -> mergeFts acc ft) Map.empty
          & showFt

      -- (_, dtype) = head dtypes
      -- (ft, t) (convert dtype Map.empty)

      !_ = unsafePerformIO $
        -- formatHaskellValue "some sensible label" (typediffs) :: IO ()
        putStrLn $ T.unpack $ typediffs

    Result.ok ()


unionWithKey f = Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMatched f)


mergeElmFileText k ft1 ft2 =
  ElmFileText
    { imports = imports ft1 <> imports ft2
    , types = types ft1 <> types ft2
    }

mergeFts :: ElmFilesText -> ElmFilesText -> ElmFilesText
mergeFts ft1 ft2 =
  unionWithKey mergeElmFileText ft1 ft2

mergeImports :: ElmImports -> ElmImports -> ElmImports
mergeImports i1 i2 =
  unionWithKey (\k v1 v2 -> v1) i1 i2

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


showFt ft =
  ft
    & Map.toList
    & List.map (\(file, ef@(ElmFileText imports types)) ->
      [ "---------------------------------------------------------------------------------"
      , "module " <> file <> " exposing (..)"
      , imports
          & fmap (\imp -> "import " <> imp)
          & T.intercalate "\n"
      , T.intercalate "\n\n\n" types
      ]
    )
    & List.concat
    & T.intercalate "\n\n"


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


unionToFt :: ModuleName.Canonical -> (Text, Text, Text, Text) -> Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> (Text, ElmImports, ElmFilesText)
unionToFt scope (author, pkg, module_, tipe) typeName interfaces recursionMap tvarMap unionInterface params =
  let
    treat union =
      let
        tvarMap = zip (Can._u_vars union) params

        -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
        resolveTvars ps =
          ps
            & fmap (\p ->
              case p of
                Can.TVar a ->
                  case List.find (\(t,ti) -> t == a) tvarMap of
                    Just (_,ti) -> ti
                    Nothing -> p
                _ -> p
            )

        constructors =
          Can._u_alts union
            -- Sort constructors by name, this allows our diff to be stable to ordering changes
            & List.sortOn
              -- Ctor N.Name Index.ZeroBased Int [Type]
              (\(Can.Ctor name _ _ _) -> N.toString name)
            -- For each constructor
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

              -- let resolvedParams = resolveTvars params_
              -- in
              -- ( N.toText name
              -- , fmap
              --     -- For each constructor type param
              --     (\resolvedParam -> canonicalToFt scope interfaces recursionMap resolvedParam tvarMap)
              --     resolvedParams
              --
              -- )
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
      in
      ( typeName
      , imports
      , (Map.singleton module_ $
          ElmFileText
            { imports = imports & getValues
            , types = ["type " <> typeName <> "\n    = " <> ctypes]
            })
          & mergeFts fts
      )

  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


aliasToFt :: ModuleName.Canonical -> (Text, Text, Text, Text) -> Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> Interface.Alias -> SnapRes
aliasToFt scope identifier@(author, pkg, module_, tipe) typeName interfaces recursionMap aliasInterface =
  let
    treat a =
      case a of
        Can.Alias tvars tipe ->
          -- let
          --   !_ = formatHaskellValue "aliasToFt" tvars :: IO ()
          -- in
          -- @TODO couldn't force an issue here but it seems wrong to ignore the tvars...
          -- why is aliasToFt never called recursively from canonicalToFt?
          -- it seems like it should be.

          let
            (subt, imps, subft) = canonicalToFt scope interfaces recursionMap tipe []

            -- imports =
            --   case identifier of
            --     ("author", "project", "Types", _) ->
            --       []
            --
            --     (author, pkg, module_, tipe) ->
            --       [module_]

          in
          -- (typeName <> " " <> subt, subft)

          ( typeName <> " " <> subt
          , Map.singleton "Huh?" "What should go here..."
          , (Map.singleton module_ $
              ElmFileText
                { imports = getValues imps
                , types = ["type alias " <> typeName <> " =" <> subt]
                })
              & mergeFts subft
          )



  in
  case aliasInterface of
    Interface.PublicAlias a -> treat a
    Interface.PrivateAlias a -> treat a


canonicalToFt :: ModuleName.Canonical -> Interface.Interfaces -> [(ModuleName.Canonical, N.Name)] -> Can.Type -> [(N.Name, Can.Type)] -> SnapRes
canonicalToFt scope interfaces recursionMap canonical tvarMap =
  let
    ft = Map.empty
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

      -- if (List.any ((==) recursionIdentifier) recursionMap) then
      --   ft
      --   -- DRecursion $ case (moduleName, name) of
      --   --   ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) (N.Name module_)), N.Name typename) ->
      --   --     pkg1 <> "/" <> pkg2 <> ":" <> module_ <> "." <> typename
      --
      -- else
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
              ("Maybe " <> subt, imps, subft)
              -- DMaybe (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossiible multi-param Maybe! Please report this."

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("List " <> subt, imps, subft)
              -- DList (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossiible multi-param List! Please report this."

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("Array " <> subt, imps, subft)
              -- DArray (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossiible multi-param Array! Please report this."

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap p tvarMap)
              in
              ("Set " <> subt, imps, subft)
              -- DSet (canonicalToFt scope interfaces recursionMap p tvarMap)
            _ ->
              error "Fatal: impossiible multi-param Set! Please report this."

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              let
                (subt, imps, subft) = (canonicalToFt scope interfaces recursionMap result tvarMap)
                (subt2, imps2, subft2) = (canonicalToFt scope interfaces recursionMap err tvarMap)
              in
              ("Result " <> subt <> " " <> subt2, mergeImports imps imps2, mergeFts subft subft2)
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
              ("Dict " <> subt <> " " <> subt2, mergeImports imps imps2, mergeFts subft subft2)
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
                    & (\(n, imports, ft) ->
                      ( module_ <> "." <> n
                      , Map.empty
                      , imports
                          & foldl (\ft imp -> addImport scope (imp <> "(utopinc)") ft) ft
                          & addImport scope (module_ <> "(utop)")
                      )
                    )

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToFt moduleName (author, pkg, module_, tipe) (N.toText name) interfaces newRecursionMap alias
                        & (\(n, imports, ft) ->
                          ( module_ <> "." <> n
                          , Map.empty
                          , imports
                              & foldl (\ft imp -> addImport scope (imp <> "(atopinc)") ft) ft
                              & addImport scope (module_ <> "(atop)")
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
      case aliasType of
        Can.Holey cType ->
          let
            tvarResolvedMap =
              tvarMap_
                & fmap (\(n,p) ->
                  case p of
                    Can.TVar a ->
                      case List.find (\(t,ti) -> t == a) tvarMap of
                        Just (_,ti) -> (n,ti)
                        Nothing -> (n,p)
                    _ -> (n,p)
                )


            (subt, imps, subft) = canonicalToFt scope interfaces recursionMap cType tvarResolvedMap

            -- !_ = formatHaskellValue "Can.TAlias.Holey" (cType, tvarMap_) :: IO ()
          in
          (subt, imps, subft)


        Can.Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?

          let
            (subt, imps, subft) = canonicalToFt scope interfaces recursionMap cType []
          in
          (subt, imps, subft)


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

          in
          ("\n    { " <> fieldsFormatted <> "\n    }"
          , Map.empty
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
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          let
            -- !_ = formatHaskellValue "Can.Tvar" ti :: IO ()
          in
          canonicalToFt scope interfaces recursionMap ti tvarMap

        Nothing ->
          ("XXXXXX Tvar Nothing", Map.empty, ft)
          -- DError $ "Error: tvar lookup failed, please report this issue: cannot find "
          --   <> N.toText name
          --   <> " in tvarMap "
          --   <>  (T.pack $ show tvarMap)

    Can.TLambda _ _ ->
      ("XXXXXX TLambda", Map.empty, ft)
      -- DError $ "must not contain functions"
