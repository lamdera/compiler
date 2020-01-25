{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Wire.TypeHash where

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


maybeGenHashes pkg canonical module_@(Valid.Module name _ _ _ _ _ _ _ _ _) interfaces = do
  onlyWhen (pkg == (Pkg.Name "author" "project") && name == N.fromText "Types") $ do
    let
      interfaceTypes_elm =
        case Map.lookup (Module.Canonical (Pkg.Name "author" "project") "Types") interfaces of
          Just i -> i
          Nothing -> error "The impossible happened, could not find src/Types.elm"

      typediffs =
        lamderaTypes
          & fmap (\t -> (t, diffableTypeByName interfaces t name interfaceTypes_elm))

      hashes =
        typediffs
          & fmap (\(t, td) -> diffableTypeToHash td)

      errors =
        typediffs
          & fmap (\(t,tds) -> (t, diffableTypeErrors tds))
          & filter (\(t,errs) -> List.length errs > 0)

    if List.length errors > 0 then
      let
        formattedErrors =
          errors
            & fmap (\(tipe, errors) ->
                D.stack $ [D.fillSep [ D.yellow $ D.fromText $ tipe <> ":" ]] ++
                  (fmap (\e -> D.fromText $ "- " <> e) errors)
              )
      in
      Result.throw $ Error.Lamdera $ LamderaError.LamderaGenericError $
        D.stack
          ([ D.reflow $ "I ran into the following problems when checking Lamdera core types:"
          -- , D.fillSep [ D.yellow "WARNING:","Confirm","hoist!" ]
          -- , D.reflow $ show errors
          ] ++ formattedErrors ++
          [ D.reflow "See <https://dashboard.lamdera.app/docs/wire> for more info."
          ])

    else do
      unsafePerformIO $ do
        root <- getProjectRoot
        writeUtf8 (lamderaHashesPath root) $ T.pack $ show hashes
        pure $ Result.ok ()



diffableTypeByName interfaces typeName name interface = do
  case Map.lookup (N.fromText typeName) $ Interface._aliases interface of
    Just alias -> do
      let diffableAlias = aliasToDiffableType interfaces alias

          -- !x = formatHaskellValue "diffableTypeByName.Alias" diffableAlias :: IO ()

      diffableAlias

    Nothing ->
      -- Try unions
      case Map.lookup (N.fromText typeName) $ Interface._unions interface of
        Just union -> do
          let diffableUnion = unionToDiffableType interfaces union []

              -- !y = formatHaskellValue "diffableTypeByName.Union" diffableUnion :: IO ()

          diffableUnion

        Nothing ->
          DError $ "Found no type named " <> typeName <> " in " <> N.toText name


lamderaTypes :: [Text]
lamderaTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


data DiffableType
  = DRecord [(Text, DiffableType)]
  | DCustom [(Text, [DiffableType])]
  | DError Text
  | DString
  | DInt
  | DFloat
  | DBool
  | DOrder
  | DNever
  | DChar
  | DMaybe DiffableType
  | DList DiffableType
  | DArray DiffableType
  | DSet DiffableType
  | DResult DiffableType DiffableType
  | DDict DiffableType DiffableType
  | DTuple DiffableType DiffableType
  | DUnit
  deriving (Show)


unionToDiffableType :: (Map.Map Canonical Module.Interface) -> Interface.Union -> [Can.Type] -> DiffableType
unionToDiffableType interfaces unionInterface params =
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
        & fmap (\(Can.Ctor name index int params_) ->
          (N.toText name, fmap (\p -> canonicalToDiffableType interfaces p []) (resolveTvars params_))
        )
        & DCustom
  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


aliasToDiffableType :: (Map.Map Canonical Module.Interface) -> Interface.Alias -> DiffableType
aliasToDiffableType interfaces aliasInterface =
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
          canonicalToDiffableType interfaces tipe []
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
canonicalToDiffableType interfaces canonical tvarMap =
  case canonical of
    Can.TType moduleName name params ->
      let
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
      in
      case (moduleName, name) of
        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "String")), N.Name "String") ->
          DString

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Basics")), N.Name "Int") ->
          DInt

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Basics")), N.Name "Float") ->
          DFloat

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Basics")), N.Name "Bool") ->
          DBool

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Basics")), N.Name "Order") ->
          DOrder

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Basics")), N.Name "Never") ->
          DNever

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Char")), N.Name "Char") ->
          DChar

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Maybe")), N.Name "Maybe") ->
          case tvarResolvedParams of
            p:[] ->
              DMaybe (canonicalToDiffableType interfaces p tvarMap)

            _ ->
              DError (N.toText "❗️impossiible multi-param Maybe")

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "List")), N.Name "List") ->
          case tvarResolvedParams of
            p:[] ->
              DList (canonicalToDiffableType interfaces p tvarMap)
            _ ->
              DError (N.toText "❗️impossiible multi-param List")

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Array")), N.Name "Array") ->
          case tvarResolvedParams of
            p:[] ->
              DArray (canonicalToDiffableType interfaces p tvarMap)
            _ ->
              DError (N.toText "❗️impossiible multi-param Array")

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Set")), N.Name "Set") ->
          case tvarResolvedParams of
            p:[] ->
              DSet (canonicalToDiffableType interfaces p tvarMap)
            _ ->
              DError (N.toText "❗️impossiible multi-param Set")

        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Result")), N.Name "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              DResult (canonicalToDiffableType interfaces result tvarMap) (canonicalToDiffableType interfaces err tvarMap)
            _ ->
              DError (N.toText "❗️impossible !2 param Result type")


        ((ModuleName.Canonical (Pkg.Name "elm" "core") (N.Name "Dict")), N.Name "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              DDict (canonicalToDiffableType interfaces result tvarMap) (canonicalToDiffableType interfaces err tvarMap)
            _ ->
              DError (N.toText "❗️impossible !2 param Dict type")


        -- These matches used to be to protect fallthrough while the full recursive flattening was being implemented
        -- Now they seem to be okay. We still need to expand the test file to play with all native modules
        -- ((ModuleName.Canonical (Pkg.Name "elm" "time") (N.Name "Time")), N.Name "Posix") ->
        --   DCustom [("Posix", [DInt])]
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm" "http") (N.Name "Http")), N.Name "Error") ->
        --   DError "Http.Error"
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm" "browser") (N.Name "Browser.Navigation")), N.Name "Key") ->
        --   DError "Browser.Navigation.Key"
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm" "url") (N.Name "Url")), N.Name "Protocol") ->
        --   DError "Url.Protocol"
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm" "browser") (N.Name "Browser")), N.Name "UrlRequest") ->
        --   DError "Browser.UrlRequest"
        --
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm" _) (N.Name n)), _) ->
        --   DError $ "❗️unhandled elm type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)
        --
        -- ((ModuleName.Canonical (Pkg.Name "elm-explorations" _) (N.Name n)), _) ->
        --   DError $ "❗️unhandled elm-explorations type: " <> (T.pack $ show moduleName) <> ":" <> (T.pack $ show name)


        -- Anything else must not be a core type, recurse once to find it
        -- @TODO how to prevent infinite recursion here if a bottom is not found, because of a new core type for example, or one you missed...?
        _ ->

          case Map.lookup moduleName interfaces of
            Just subInterface ->

              -- Try unions
              case Map.lookup name $ Interface._unions subInterface of
                Just union -> do
                  unionToDiffableType interfaces union tvarResolvedParams

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToDiffableType interfaces alias

                    Nothing ->
                      DError $ "❗️impossible: failed to find either alias or custom type for type that must exist: " <> (T.pack $ show name)

            Nothing ->
              DError $ "❗️impossible: failed to lookup interface for module that must exist: " <> (T.pack $ show moduleName)


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
          canonicalToDiffableType interfaces cType tvarResolvedMap

        Can.Filled cType ->
          canonicalToDiffableType interfaces cType []

    Can.TRecord fieldMap maybeName_whatisit ->
      let
        -- !_ = formatHaskellValue "Can.TRecord" (fieldMap, tvarMap) :: IO ()
      in
      fieldMap
        & Map.toList
        & fmap (\(name,(Can.FieldType index tipe)) ->
          (N.toText name, canonicalToDiffableType interfaces tipe tvarMap)
        )
        & DRecord

    Can.TTuple firstType secondType maybeType_whatisthisfor ->
      DTuple (canonicalToDiffableType interfaces firstType tvarMap) (canonicalToDiffableType interfaces secondType tvarMap)

    Can.TUnit ->
      DUnit

    Can.TVar name ->
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          let
            -- !_ = formatHaskellValue "Can.Tvar" ti :: IO ()
          in
          canonicalToDiffableType interfaces ti tvarMap

        Nothing ->
          DError $ "Error: tvar lookup failed, please report issue along with your type!: " <> N.toText name <> " in tvarMap " <>  (T.pack $ show tvarMap)

    Can.TLambda _ _ ->
      DError $ "must not contain functions"


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
    DCustom constructors ->
      constructors
        & fmap (\(n, params) -> T.intercalate "" $ fmap diffableTypeToText params)
        & T.intercalate ""
        & (\v -> "Custom["<> v <>"]")

    -- DError Text
    DError error ->
      "[ERROR]"

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
    DCustom constructors ->
      constructors
        & fmap (\(n, params) ->
            fmap diffableTypeErrors params
              & List.concat
          )
        & List.concat

    -- DError Text
    DError error ->
      [error]

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
