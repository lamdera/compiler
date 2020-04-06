{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen where




{- Hashes for Elm types
@TODO move into Evergreen namespace
-}

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


-- import Wire.TypeHash


data ElmFileText =
  ElmFileText
    { imports :: [Text]
    , types :: [Text]
    } deriving (Show)

type ElmFilesText = Map Text ElmFileText
--
-- snapshotCurrentTypes :: FilePath -> Int -> [(Text, DiffableType)] -> IO String
-- snapshotCurrentTypes root version dtypes = do
--
--   let
--     (_, dtype) = head dtypes
--
--
--     (ft, t) (convert dtype Map.empty)
--
--     !_ = formatHaskellValue "some sensible label" (ft, t) :: IO ()
--
--
--   pure ""
--
--   -- -- Snapshot the current types, and rename the module for the snapshot
--   -- _ <- mkdir $ root </> "src/Evergreen/Type"
--   -- let nextType = (root </> "src/Evergreen/Type/V") <> show version <> ".elm"
--   -- debug $ "Snapshotting current types to " <> nextType
--   -- _ <- readProcess "cp" [ root </> "src/Types.elm", nextType ] ""
--   -- -- osReplace ("s/module Types exposing/module Evergreen.Type.V" <> show version <> " exposing/g") nextType
--   -- replaceInFile "module Types exposing" ("module Evergreen.Type.V" <> tshow version <> " exposing") nextType
--   -- pure ""
--
-- convert :: DiffableType -> ElmFilesText -> (ElmFilesText, Text)
-- convert dtype ft =
--   case dtype of
--     -- DRecord fields ->
--     --   fields
--     --     & fmap (\(n, tipe) -> diffableTypeExternalWarnings tipe)
--     --     & List.concat
--     --
--     -- -- DCustom [(Text, [DiffableType])]
--     -- DCustom name constructors ->
--     --   constructors
--     --     & fmap (\(n, params) ->
--     --         fmap diffableTypeExternalWarnings params
--     --           & List.concat
--     --       )
--     --     & List.concat
--     --
--     DString ->
--       (ft, "String")
--     --
--     -- DInt ->
--     --   ft
--     --
--     -- DFloat ->
--     --   ft
--     --
--     -- DBool ->
--     --   ft
--     --
--     -- DOrder ->
--     --   ft
--     --
--     -- DNever ->
--     --   ft
--     --
--     -- DChar ->
--     --   ft
--     --
--     -- -- DMaybe DiffableType
--     -- DMaybe tipe ->
--     --   diffableTypeExternalWarnings tipe
--     --
--     -- -- DList DiffableType
--     -- DList tipe ->
--     --   diffableTypeExternalWarnings tipe
--     --
--     -- -- DArray DiffableType
--     -- DArray tipe ->
--     --   diffableTypeExternalWarnings tipe
--     --
--     -- -- DSet DiffableType
--     -- DSet tipe ->
--     --   diffableTypeExternalWarnings tipe
--     --
--     -- -- DResult DiffableType DiffableType
--     -- DResult err result ->
--     --   -- []
--     --   diffableTypeExternalWarnings err ++ diffableTypeExternalWarnings result
--     --
--     -- -- DDict DiffableType DiffableType
--     -- DDict key value ->
--     --   -- []
--     --   diffableTypeExternalWarnings key ++ diffableTypeExternalWarnings value
--     --
--     -- -- DTuple DiffableType DiffableType
--     -- DTuple first second ->
--     --   -- []
--     --   diffableTypeExternalWarnings first ++ diffableTypeExternalWarnings second
--     --
--     -- DUnit ->
--     --   []
--     --
--     -- DRecursion name ->
--     --   []
--     --
--     -- -- DError Text
--     -- DError error ->
--     --   []
--     --
--     -- DExternalWarning (author, pkg, module_, tipe) realtipe ->
--     --   [ module_ <> "." <> tipe <> " (" <> author <> "/" <> pkg <> ")"]
--     --     ++ diffableTypeExternalWarnings realtipe
--
--     d ->
--       (ft, tshow d)



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
          & foldl (\acc (t, ft) -> unionWithKey mergeElmFileText acc ft) Map.empty
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

showFt ft =
  ft
    & Map.toList
    & List.map (\(file, ef@(ElmFileText imports types)) ->
      [ "module " <> file <> " exposing (..)"
      , T.intercalate "\n" imports
      , T.intercalate "\n\n\n" types
      ]
    )
    & List.concat
    & T.intercalate "\n\n"


ftByName :: (Map.Map Canonical Module.Interface) -> Text -> N.Name -> Interface.Interface -> ElmFilesText
ftByName interfaces typeName name interface = do
  let
    recursionIdentifier =
      ((ModuleName.Canonical (Pkg.Name "author" "project") (N.Name "Types")), N.Name typeName)

    identifier =
      case recursionIdentifier of
        ((ModuleName.Canonical (Pkg.Name author pkg) (N.Name module_)), N.Name tipe) ->
          (author, pkg, module_, tipe)


  case Map.lookup (N.fromText typeName) $ Interface._aliases interface of
    Just alias -> do
      let
        diffableAlias = aliasToFt typeName interfaces [recursionIdentifier] alias

        -- !x = formatHaskellValue "ftByName.Alias" diffableAlias :: IO ()

      diffableAlias

    Nothing ->
      -- Try unions
      case Map.lookup (N.fromText typeName) $ Interface._unions interface of
        Just union -> do
          let
            diffableUnion = unionToFt identifier typeName interfaces [recursionIdentifier] [] union []

            -- !y = formatHaskellValue "ftByName.Union" diffableUnion :: IO ()

          diffableUnion

        Nothing ->
          Map.empty
          -- DError $ "Found no type named " <> typeName <> " in " <> N.toText name


unionToFt :: (Text, Text, Text, Text) -> Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> ElmFilesText
unionToFt (author, pkg, module_, tipe) typeName interfaces recursionMap tvarMap unionInterface params =
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

              ( N.toText name <> " " <> (fmap (\param -> "TODOPARAM") params_ & T.intercalate " ")
              , fmap
                  -- For each constructor type param
                  (\param -> canonicalToFt interfaces recursionMap param tvarMap)
                  params_

              )

              -- let resolvedParams = resolveTvars params_
              -- in
              -- ( N.toText name
              -- , fmap
              --     -- For each constructor type param
              --     (\resolvedParam -> canonicalToFt interfaces recursionMap resolvedParam tvarMap)
              --     resolvedParams
              --
              -- )
            )

        ctypes =
          constructors
           & fmap (\(t,ft) -> t)
           & T.intercalate "\n    | "
      in
      Map.singleton module_ $
        ElmFileText
          { imports = []
          , types = ["type " <> typeName <> "\n    = " <> ctypes]
          }



  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


aliasToFt :: Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> Interface.Alias -> ElmFilesText
aliasToFt typeName interfaces recursionMap aliasInterface =
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
          canonicalToFt interfaces recursionMap tipe []
  in
  case aliasInterface of
    Interface.PublicAlias a -> treat a
    Interface.PrivateAlias a -> treat a


-- = TLambda Type Type
-- | TVar N.Name
-- | TType ModuleName.Canonical N.Name [Type]
-- | TRecord (Map.Map N.Name FieldType) (Maybe N.Name)
-- | TUnit
-- | TTuple Type Type (Maybe Type)
-- | TAlias ModuleName.Canonical N.Name [(N.Name, Type)] AliasType
canonicalToFt :: Interface.Interfaces -> [(ModuleName.Canonical, N.Name)] -> Can.Type -> [(N.Name, Can.Type)] -> ElmFilesText
canonicalToFt interfaces recursionMap canonical tvarMap =
  let ft = Map.empty
  in
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
              ft
              -- DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_
      in

      if (List.any ((==) recursionIdentifier) recursionMap) then
        ft
        -- DRecursion $ case (moduleName, name) of
        --   ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) (N.Name module_)), N.Name typename) ->
        --     pkg1 <> "/" <> pkg2 <> ":" <> module_ <> "." <> typename

      else
      case identifier of
        ("elm", "core", "String", "String") ->
          ft
          -- DString

        ("elm", "core", "Basics", "Int") ->
          ft
          -- DInt

        ("elm", "core", "Basics", "Float") ->
          ft
          -- DFloat

        ("elm", "core", "Basics", "Bool") ->
          ft
          -- DBool

        ("elm", "core", "Basics", "Order") ->
          ft
          -- DOrder

        ("elm", "core", "Basics", "Never") ->
          ft
          -- DNever

        ("elm", "core", "Char", "Char") ->
          ft
          -- DChar

        ("elm", "core", "Maybe", "Maybe") ->
          case tvarResolvedParams of
            p:[] ->
              ft
              -- DMaybe (canonicalToFt interfaces recursionMap p tvarMap)

            _ ->
              ft
              -- DError "❗️impossiible multi-param Maybe"

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              ft
              -- DList (canonicalToFt interfaces recursionMap p tvarMap)
            _ ->
              ft
              -- DError "❗️impossiible multi-param List"

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              ft
              -- DArray (canonicalToFt interfaces recursionMap p tvarMap)
            _ ->
              ft
              -- DError "❗️impossiible multi-param Array"

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              ft
              -- DSet (canonicalToFt interfaces recursionMap p tvarMap)
            _ ->
              ft
              -- DError "❗️impossiible multi-param Set"

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              ft
              -- DResult (canonicalToFt interfaces recursionMap result tvarMap) (canonicalToFt interfaces recursionMap err tvarMap)
            _ ->
              ft
              -- DError "❗️impossible !2 param Result type"


        ("elm", "core", "Dict", "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              ft
              -- DDict (canonicalToFt interfaces recursionMap result tvarMap) (canonicalToFt interfaces recursionMap err tvarMap)
            _ ->
              ft
              -- DError "❗️impossible !2 param Dict type"


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
                  unionToFt (author, pkg, module_, tipe) (N.toText name) interfaces newRecursionMap tvarMap union tvarResolvedParams
                    & addExternalWarning (author, pkg, module_, tipe)

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToFt "FIXME-ALIAS-TYPENAME" interfaces newRecursionMap alias
                        & addExternalWarning (author, pkg, module_, tipe)

                    Nothing ->
                      ft
                      -- DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_ <> ". Please report this issue with your code!"

            Nothing ->
              ft
              -- let !_ = formatHaskellValue "interface modulenames" (Map.keys interfaces) :: IO ()
              -- in
              -- DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


    Can.TAlias moduelName name tvarMap_ aliasType ->
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

            -- !_ = formatHaskellValue "Can.TAlias.Holey" (cType, tvarMap_) :: IO ()
          in
          canonicalToFt interfaces recursionMap cType tvarResolvedMap

        Can.Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
          canonicalToFt interfaces recursionMap cType []

    Can.TRecord fieldMap isPartial ->
      case isPartial of
        Just whatIsThis ->
          ft
          -- DError "must not contain partial records"

        Nothing ->
          ft
          -- let
          --   -- !_ = formatHaskellValue "Can.TRecord" (fieldMap, tvarMap) :: IO ()
          -- in
          -- fieldMap
          --   & Map.toList
          --   & fmap (\(name,(Can.FieldType index tipe)) ->
          --     (N.toText name, canonicalToFt interfaces recursionMap tipe tvarMap)
          --   )
          --   & DRecord

    Can.TTuple firstType secondType maybeType_whatisthisfor ->
      ft
      -- DTuple (canonicalToFt interfaces recursionMap firstType tvarMap) (canonicalToFt interfaces recursionMap secondType tvarMap)

    Can.TUnit ->
      ft

    Can.TVar name ->
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          let
            -- !_ = formatHaskellValue "Can.Tvar" ti :: IO ()
          in
          canonicalToFt interfaces recursionMap ti tvarMap

        Nothing ->
          ft
          -- DError $ "Error: tvar lookup failed, please report this issue: cannot find "
          --   <> N.toText name
          --   <> " in tvarMap "
          --   <>  (T.pack $ show tvarMap)

    Can.TLambda _ _ ->
      ft
      -- DError $ "must not contain functions"


-- Any types that are outside user's Types.elm need a warning currently
addExternalWarning :: (Text,Text,Text,Text) -> ElmFilesText -> ElmFilesText
addExternalWarning (author, pkg, module_, tipe) eft =
  eft
  -- case (author, pkg, module_, tipe) of
  --   ("author", "project", "Types", _) ->
  --     dtype
  --
  --   _ ->
  --     DExternalWarning (author, pkg, module_, tipe) dtype
