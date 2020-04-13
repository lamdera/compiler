{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.TypeHash where

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


import Lamdera
import Lamdera.Types
import qualified Lamdera.Evergreen


lamderaTypes :: [Text]
lamderaTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


maybeGenHashes :: Pkg.Name -> Valid.Module -> Interface.Interfaces -> Result.Result i w Error.Error ()
maybeGenHashes pkg module_@(Valid.Module name _ _ _ _ _ _ _ _ _) interfaces = do

  let !inDebug = unsafePerformIO Lamdera.isDebug

  -- This check isn't strictly required, as the callee of this function in compile only
  -- calls it when we know we've canonicalized the src/Types.elm file, but leaving it here
  -- to prevent any footguns in future testing
  debug_note "Generating type hashes..." $
   onlyWhen (pkg == (Pkg.Name "author" "project") && name == N.fromText "Types") $ do
    let
      interfaceTypes_elm =
        case Map.lookup (Module.Canonical (Pkg.Name "author" "project") "Types") interfaces of
          Just i -> i
          Nothing -> error "The impossible happened, could not find src/Types.elm"

      typediffs =
        lamderaTypes
          & fmap (\t -> (t, diffableTypeByName interfaces t name interfaceTypes_elm))

      -- !_ = formatHaskellValue "typediffs" (typediffs) :: IO ()

      hashes =
        typediffs
          & fmap (\(t, td) -> diffableTypeToHash td)

      errors =
        typediffs
          & fmap (\(t,tds) -> (t, diffableTypeErrors tds, tds))
          & filter (\(t,errs,tds) -> List.length errs > 0)

      warnings =
        typediffs
          & fmap (\(t,tds) -> (t, diffableTypeExternalWarnings tds & List.nub, tds)) -- nub == unique
          & filter (\(t,errs,tds) -> List.length errs > 0)

      formattedWarnings =
        warnings
          & fmap (\(tipe, warnings_, tds) ->
              D.stack $
                [D.fillSep [ D.yellow $ D.fromText $ tipe <> " includes:" ]]
                ++
                (fmap (\e -> D.fromText $ "- " <> e) warnings_)
            )

      textWarnings =
        warnings
          & fmap (\(tipe, warnings_, tds) ->
              [ tipe <> " includes:\n\n" ]
              ++
              (warnings_
                & fmap (\e -> "- " <> e)
                & List.intersperse "\n"
              )
            )
          & List.intersperse ["\n\n"]
          & List.concat
          & T.intercalate ""

      formattedErrors =
        errors
          & fmap (\(tipe, errors_, tds) ->
              D.stack $
                [D.fillSep [ D.yellow $ D.fromText $ tipe <> ":" ]]
                ++
                (fmap (\e -> D.fromText $ "- " <> e) errors_)
            )

    if List.length errors > 0
      then
      let
        !x = onlyWhen inDebug $ formatHaskellValue "diffHasErrors:" typediffs :: IO ()

        notifyWarnings =
          if List.length warnings > 0 then
            [ D.reflow $ "Alpha warning: also, a number of types outside Types.elm are referenced, see `lamdera check` for more info." ]
          else
            []
      in
      Result.throw $ Error.Lamdera $ LamderaError.LamderaGenericError $
        D.stack
          ([ D.reflow $ "I ran into the following problems when checking Lamdera core types:"
          -- , D.fillSep [ D.yellow "WARNING:","Confirm","hoist!" ]
          -- , D.reflow $ show errors
          ] ++ formattedErrors ++
          [ D.reflow "See <https://dashboard.lamdera.app/docs/wire> for more info."
          ] ++ notifyWarnings)

      else do
        unsafePerformIO $ do
          root <- getProjectRoot
          writeUtf8 (lamderaHashesPath root) $ T.pack $ show hashes

          if (List.length formattedWarnings > 0)
            then do
              writeUtf8 (lamderaExternalWarningsPath root) $ textWarnings
            else
              remove (lamderaExternalWarningsPath root)

          pure $ Result.ok ()


diffableTypeByName :: (Map.Map Canonical Module.Interface) -> Text -> N.Name -> Interface.Interface -> DiffableType
diffableTypeByName interfaces typeName name interface = do
  let
    recursionIdentifier =
      ((ModuleName.Canonical (Pkg.Name "author" "project") (N.Name "Types")), N.Name typeName)

  case Map.lookup (N.fromText typeName) $ Interface._aliases interface of
    Just alias -> do
      let
        diffableAlias = aliasToDiffableType interfaces [recursionIdentifier] alias

        -- !x = formatHaskellValue "diffableTypeByName.Alias" diffableAlias :: IO ()

      diffableAlias

    Nothing ->
      -- Try unions
      case Map.lookup (N.fromText typeName) $ Interface._unions interface of
        Just union -> do
          let
            diffableUnion = unionToDiffableType typeName interfaces [recursionIdentifier] [] union []

            -- !y = formatHaskellValue "diffableTypeByName.Union" diffableUnion :: IO ()

          diffableUnion

        Nothing ->
          DError $ "Found no type named " <> typeName <> " in " <> N.toText name


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToDiffableType :: Text -> (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> DiffableType
unionToDiffableType typeName interfaces recursionMap tvarMap unionInterface params =
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
      in
      Can._u_alts union
        -- Sort constructors by name, this allows our diff to be stable to ordering changes
        & List.sortOn
          -- Ctor N.Name Index.ZeroBased Int [Type]
          (\(Can.Ctor name _ _ _) -> N.toString name)
        -- For each constructor
        & fmap (\(Can.Ctor name index int params_) ->
          ( N.toText name
          , -- For each constructor type param
            params_
              & resolveTvars
              & fmap (\resolvedParam -> canonicalToDiffableType interfaces recursionMap resolvedParam tvarMap )
          )
        )
        & DCustom typeName
  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


-- A top level Alias definition i.e. `type alias ...`
aliasToDiffableType :: (Map.Map Canonical Module.Interface) -> [(ModuleName.Canonical, N.Name)] -> Interface.Alias -> DiffableType
aliasToDiffableType interfaces recursionMap aliasInterface =
  let
    treat a =
      case a of
        Can.Alias tvars tipe ->
          -- let
          --   !_ = formatHaskellValue "aliasToDiffableType" tvars :: IO ()
          -- in
          -- @TODO couldn't force an issue here but it seems wrong to ignore the tvars...
          -- why is aliasToDiffableType never called recursively from canonicalToDiffableType?
          -- it seems like it should be.
          canonicalToDiffableType interfaces recursionMap tipe []
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
canonicalToDiffableType :: Interface.Interfaces -> [(ModuleName.Canonical, N.Name)] -> Can.Type -> [(N.Name, Can.Type)] -> DiffableType
canonicalToDiffableType interfaces recursionMap canonical tvarMap =
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
              DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_
      in

      if (List.any ((==) recursionIdentifier) recursionMap) then
        DRecursion $ case (moduleName, name) of
          ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) (N.Name module_)), N.Name typename) ->
            pkg1 <> "/" <> pkg2 <> ":" <> module_ <> "." <> typename

      else
      case identifier of
        ("elm", "core", "String", "String") ->
          DString

        ("elm", "core", "Basics", "Int") ->
          DInt

        ("elm", "core", "Basics", "Float") ->
          DFloat

        ("elm", "core", "Basics", "Bool") ->
          DBool

        ("elm", "core", "Basics", "Order") ->
          DOrder

        ("elm", "core", "Basics", "Never") ->
          DNever

        ("elm", "core", "Char", "Char") ->
          DChar

        ("elm", "core", "Maybe", "Maybe") ->
          case tvarResolvedParams of
            p:[] ->
              DMaybe (canonicalToDiffableType interfaces recursionMap p tvarMap)

            _ ->
              DError "❗️impossible multi-param Maybe"

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              DList (canonicalToDiffableType interfaces recursionMap p tvarMap)
            _ ->
              DError "❗️impossible multi-param List"

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              DArray (canonicalToDiffableType interfaces recursionMap p tvarMap)
            _ ->
              DError "❗️impossible multi-param Array"

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              DSet (canonicalToDiffableType interfaces recursionMap p tvarMap)
            _ ->
              DError "❗️impossible multi-param Set"

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              DResult (canonicalToDiffableType interfaces recursionMap result tvarMap) (canonicalToDiffableType interfaces recursionMap err tvarMap)
            _ ->
              DError "❗️impossible !2 param Result type"


        ("elm", "core", "Dict", "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              DDict (canonicalToDiffableType interfaces recursionMap result tvarMap) (canonicalToDiffableType interfaces recursionMap err tvarMap)
            _ ->
              DError "❗️impossible !2 param Dict type"


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
                  unionToDiffableType (N.toText name) interfaces newRecursionMap tvarMap union tvarResolvedParams
                    & addExternalWarning (author, pkg, module_, tipe)

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToDiffableType interfaces newRecursionMap alias
                        & addExternalWarning (author, pkg, module_, tipe)

                    Nothing ->
                      DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_ <> ". Please report this issue with your code!"

            Nothing ->
              -- let !_ = formatHaskellValue "interface modulenames" (Map.keys interfaces) :: IO ()
              -- in
              DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


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

            -- !_ = formatHaskellValue ("Can.TAlias.Holey - " <> N.toString name) (cType, tvarMap_) :: IO ()
          in
          canonicalToDiffableType interfaces recursionMap cType tvarResolvedMap

        Can.Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
          canonicalToDiffableType interfaces recursionMap cType []

    Can.TRecord fieldMap isPartial ->
      case isPartial of
        Just whatIsThis ->
          DError "must not contain partial records"

        Nothing ->
          let
            -- !_ = formatHaskellValue "Can.TRecord" (fieldMap, tvarMap) :: IO ()
          in
          fieldMap
            & Map.toList
            & fmap (\(name,(Can.FieldType index tipe)) ->
              (N.toText name, canonicalToDiffableType interfaces recursionMap tipe tvarMap)
            )
            & DRecord

    Can.TTuple firstType secondType maybeType_whatisthisfor ->
      DTuple (canonicalToDiffableType interfaces recursionMap firstType tvarMap) (canonicalToDiffableType interfaces recursionMap secondType tvarMap)

    Can.TUnit ->
      DUnit

    Can.TVar name ->
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          let
            -- !_ = formatHaskellValue "Can.Tvar" ti :: IO ()
          in
          canonicalToDiffableType interfaces recursionMap ti tvarMap

        Nothing ->
          DError $ "Error: tvar lookup failed, please report this issue: cannot find "
            <> N.toText name
            <> " in tvarMap "
            <>  (T.pack $ show tvarMap)

    Can.TLambda _ _ ->
      DError $ "must not contain functions"


-- Any types that are outside user's project need a warning currently
addExternalWarning :: (Text,Text,Text,Text) -> DiffableType -> DiffableType
addExternalWarning (author, pkg, module_, tipe) dtype =
  case (author, pkg, module_, tipe) of
    ("author", "project", _, _) ->
      dtype

    _ ->
      DExternalWarning (author, pkg, module_, tipe) dtype



diffableTypeToHash :: DiffableType -> Text
diffableTypeToHash dtype =
  T.pack $ SHA.showDigest $ SHA.sha1 $ TLE.encodeUtf8 $ TL.fromStrict $ diffableTypeToText dtype


diffableTypeToText :: DiffableType -> Text
diffableTypeToText dtype =
  case dtype of
    -- DRecord [(Text, DiffableType)]
    DRecord fields ->
      fields
        & fmap (\(n, tipe) -> diffableTypeToText tipe)
        & T.intercalate ""
        & (\v -> "Rec["<> v <>"]")

    -- DCustom [(Text, [DiffableType])]
    DCustom name constructors ->
      constructors
        & fmap (\(n, params) -> T.intercalate "" $ fmap diffableTypeToText params)
        & T.intercalate ""
        & (\v -> "Custom["<> v <>"]")

    -- DString
    DString ->
      "String"

    -- DInt
    DInt ->
      "Int"

    -- DFloat
    DFloat ->
      "Float"

    -- DBool
    DBool ->
      "Bool"

    -- DOrder
    DOrder ->
      "Order"

    -- DNever
    DNever ->
      "Never"

    -- DChar
    DChar ->
      "Char"

    -- DMaybe DiffableType
    DMaybe tipe ->
      "Maybe["<> diffableTypeToText tipe <>"]"

    -- DList DiffableType
    DList tipe ->
      "List["<> diffableTypeToText tipe <>"]"

    -- DArray DiffableType
    DArray tipe ->
      "Array["<> diffableTypeToText tipe <>"]"

    -- DSet DiffableType
    DSet tipe ->
      "Set["<> diffableTypeToText tipe <>"]"

    -- DResult DiffableType DiffableType
    DResult err result ->
      "Result["<> diffableTypeToText err <>","<> diffableTypeToText result <>"]"

    -- DDict DiffableType DiffableType
    DDict key value ->
      "Dict["<> diffableTypeToText key <>","<> diffableTypeToText value <>"]"

    -- DTuple DiffableType DiffableType
    DTuple first second ->
      "Tuple["<> diffableTypeToText first <>","<> diffableTypeToText second <>"]"

    DUnit ->
      "()"

    -- DRecursion Text
    DRecursion name ->
      -- Ideally recursion should be stable to name changes, but right now we
      -- have no easy way of knowing here if a name change coincided with a type
      -- change also, so for now name changes to recursive values count as "changed"
      "Recursion[" <> name <> "]"

    -- DError Text
    DError error ->
      "[ERROR]"

    DExternalWarning _ tipe ->
      diffableTypeToText tipe


diffableTypeErrors :: DiffableType -> [Text]
diffableTypeErrors dtype =
  -- ["blahhhh"]
  case dtype of
    -- DRecord [(Text, DiffableType)]
    DRecord fields ->
      -- let
      --   !_ = formatHaskellValue "DRecord" (fields) :: IO ()
      -- in
      -- ["test"]
      fields
        & fmap (\(n, tipe) -> diffableTypeErrors tipe)
        & List.concat

    -- DCustom [(Text, [DiffableType])]
    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            fmap diffableTypeErrors params
              & List.concat
          )
        & List.concat

    -- DString
    DString ->
      []

    -- DInt
    DInt ->
      []

    -- DFloat
    DFloat ->
      []

    -- DBool
    DBool ->
      []

    -- DOrder
    DOrder ->
      []

    -- DNever
    DNever ->
      ["must not contain Never values"]

    -- DChar
    DChar ->
      []

    -- DMaybe DiffableType
    DMaybe tipe ->
      diffableTypeErrors tipe

    -- DList DiffableType
    DList tipe ->
      diffableTypeErrors tipe

    -- DArray DiffableType
    DArray tipe ->
      diffableTypeErrors tipe

    -- DSet DiffableType
    DSet tipe ->
      diffableTypeErrors tipe

    -- DResult DiffableType DiffableType
    DResult err result ->
      -- []
      diffableTypeErrors err ++ diffableTypeErrors result

    -- DDict DiffableType DiffableType
    DDict key value ->
      -- []
      diffableTypeErrors key ++ diffableTypeErrors value

    -- DTuple DiffableType DiffableType
    DTuple first second ->
      -- []
      diffableTypeErrors first ++ diffableTypeErrors second

    DUnit ->
      []

    DRecursion name ->
      []

    -- DError Text
    DError error ->
      [error]

    DExternalWarning _ realtipe ->
      -- [ author <> "/" <> pkg <> ":" <> module_ <> "." <> tipe <> " is outside Types.elm and won't get caught by Evergreen!"]
      --   ++ diffableTypeErrors realtipe
      diffableTypeErrors realtipe


diffableTypeExternalWarnings :: DiffableType -> [Text]
diffableTypeExternalWarnings dtype =
  case dtype of
    DRecord fields ->
      fields
        & fmap (\(n, tipe) -> diffableTypeExternalWarnings tipe)
        & List.concat

    -- DCustom [(Text, [DiffableType])]
    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            fmap diffableTypeExternalWarnings params
              & List.concat
          )
        & List.concat

    -- DString
    DString ->
      []

    -- DInt
    DInt ->
      []

    -- DFloat
    DFloat ->
      []

    -- DBool
    DBool ->
      []

    -- DOrder
    DOrder ->
      []

    -- DNever
    DNever ->
      []

    -- DChar
    DChar ->
      []

    -- DMaybe DiffableType
    DMaybe tipe ->
      diffableTypeExternalWarnings tipe

    -- DList DiffableType
    DList tipe ->
      diffableTypeExternalWarnings tipe

    -- DArray DiffableType
    DArray tipe ->
      diffableTypeExternalWarnings tipe

    -- DSet DiffableType
    DSet tipe ->
      diffableTypeExternalWarnings tipe

    -- DResult DiffableType DiffableType
    DResult err result ->
      -- []
      diffableTypeExternalWarnings err ++ diffableTypeExternalWarnings result

    -- DDict DiffableType DiffableType
    DDict key value ->
      -- []
      diffableTypeExternalWarnings key ++ diffableTypeExternalWarnings value

    -- DTuple DiffableType DiffableType
    DTuple first second ->
      -- []
      diffableTypeExternalWarnings first ++ diffableTypeExternalWarnings second

    DUnit ->
      []

    DRecursion name ->
      []

    -- DError Text
    DError error ->
      []

    DExternalWarning (author, pkg, module_, tipe) realtipe ->
      [ module_ <> "." <> tipe <> " (" <> author <> "/" <> pkg <> ")"]
        ++ diffableTypeExternalWarnings realtipe
