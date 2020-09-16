{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.TypeHash where

{- Hashes for Elm types
@TODO move into Evergreen namespace
-}

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N

import qualified AST.Canonical as Can
import qualified AST.Source as Valid
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import qualified Reporting.Doc as D

import Lamdera
import Lamdera.Types
import Lamdera.Progress
import qualified Lamdera.Evergreen


type Interfaces =
  Map.Map ModuleName.Canonical Interface.Interface


lamderaTypes :: [N.Name]
lamderaTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


-- @TODO restore after integration
-- maybeGenHashes :: Pkg.Name -> Valid.Module -> Interfaces -> Result.Result i w Error.Error ()
maybeGenHashes pkg module_@(Valid.Module name _ _ _ _ _ _ _ _) interfaces = do

  let
    !inDebug = unsafePerformIO Lamdera.isDebug

    moduleName =
      case name of
        Just (A.At _ name_) -> name_
        _ -> toName ""

  -- This check isn't strictly required, as the callee of this function in compile only
  -- calls it when we know we've canonicalized the src/Types.elm file, but leaving it here
  -- to prevent any footguns in future testing
  debug_note "Generating type hashes..." $
   onlyWhen (pkg == (Pkg.Name "author" "project") && moduleName == "Types") $ do
    let
      interfaceTypes_elm =
        case Map.lookup (ModuleName.Canonical (Pkg.Name "author" "project") "Types") interfaces of
          Just i -> i
          Nothing -> error "The impossible happened, could not find src/Types.elm"

      typediffs :: [(Text, DiffableType)]
      typediffs =
        lamderaTypes
          & fmap (\t -> (nameToText t, diffableTypeByName interfaces t moduleName interfaceTypes_elm))

      -- @WARNING this may freeze for a while, hindent struggles with large haskell values
      -- !_ = formatHaskellValue "typediffs" (typediffs) :: IO ()

      hashes :: [Text]
      hashes =
        typediffs
          & fmap (\(t, td) -> diffableTypeToHash td)


      errors :: [(Text, [Text], DiffableType)]
      errors =
        typediffs
          & fmap (\(t,tds) -> (t, diffableTypeErrors tds, tds))
          & filter (\(t,errs,tds) -> List.length errs > 0)

      warnings :: [(Text, [Text], DiffableType)]
      warnings =
        typediffs
          & fmap (\(t,tds) -> (t, diffableTypeExternalWarnings tds & List.nub, tds)) -- nub == unique
          & filter (\(t,errs,tds) -> List.length errs > 0)

      textWarnings :: Text
      textWarnings =
        warnings
          & fmap (\(tipe, warnings_, tds) -> warnings_)
          & List.concat
          & List.nub
          & List.sort
          & T.intercalate "\n- "
          & (<>) "- "

      formattedErrors :: [D.Doc]
      formattedErrors =
        errors
          & fmap (\(tipe, errors_, tds) ->
              D.stack $
                [D.fillSep [ D.yellow $ D.fromChars . T.unpack $ tipe <> ":" ]]
                ++
                (errors_ & List.nub & fmap (\e -> D.fromChars . T.unpack $ "- " <> e) )
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
      Result.throw $
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
          writeUtf8 (lamderaHashesPath root) $ show_ hashes

          if (List.length warnings > 0)
            then do
              writeUtf8 (lamderaExternalWarningsPath root) $ textWarnings
            else
              remove (lamderaExternalWarningsPath root)

          pure $ Result.ok ()


diffableTypeByName :: (Map.Map ModuleName.Canonical Interface.Interface) -> N.Name -> N.Name -> Interface.Interface -> DiffableType
diffableTypeByName interfaces typeName name interface = do
  let
    recursionIdentifier =
      ((ModuleName.Canonical (Pkg.Name "author" "project") "Types"), typeName)

  case Map.lookup typeName $ Interface._aliases interface of
    Just alias -> do
      let
        diffableAlias = aliasToDiffableType interfaces [recursionIdentifier] [] alias []

        -- !x = formatHaskellValue "diffableTypeByName.Alias" diffableAlias :: IO ()

      diffableAlias

    Nothing ->
      -- Try unions
      case Map.lookup typeName $ Interface._unions interface of
        Just union -> do
          let
            diffableUnion = unionToDiffableType (nameToText typeName) interfaces [recursionIdentifier] [] union []

            -- !y = formatHaskellValue "diffableTypeByName.Union" diffableUnion :: IO ()

          diffableUnion

        Nothing ->
          DError $ "Found no type named " <> nameToText typeName <> " in " <> nameToText name



-- Recursively resolve any tvars in the given type using the given tvarMap
resolveTvars_ :: [(N.Name, Can.Type)] -> Can.Type -> Can.Type
resolveTvars_ tvarMap tipe =
  case tipe of
    Can.TVar a ->
      -- We've found a tvar, attempt to make a replacement
      case List.find (\(t,ti) -> t == a) (tvarMap) of
        Just (t,ti) ->
          ti

        Nothing ->
          -- Note: this used to seem an error, but throwing an error broke cases
          -- and this makes sense – a tvar might not get resolved till a higher
          -- up usage provide the type param. So recursively searching to resolve
          -- makes sense.
          -- @TODO performance might be impacted here on large types
          tipe

    Can.TType moduleName name params ->
      Can.TType moduleName name (params & fmap (resolveTvars_ tvarMap))

    Can.TAlias moduleName name tvarMap_ aliasType ->
      case aliasType of
        Can.Holey cType ->
          Can.TAlias moduleName name tvarMap_
            (Can.Holey (resolveTvars_ tvarMap cType))

        Can.Filled cType ->
          Can.TAlias moduleName name tvarMap_
            (Can.Filled (resolveTvars_ tvarMap cType))

    Can.TRecord fieldMap isPartial ->
      case isPartial of
        Just whatIsThis ->
          error $ "Error: tvar lookup encountered unsupported partial record, please report this issue."

        Nothing ->
          let
            newFieldMap =
              fieldMap
                & Map.map (\(Can.FieldType index tipe) ->
                  Can.FieldType index (resolveTvars_ tvarMap tipe)
                )
          in
          Can.TRecord newFieldMap isPartial

    Can.TTuple firstType secondType maybeType_whatisthisfor ->
      Can.TTuple (resolveTvars_ tvarMap firstType) (resolveTvars_ tvarMap secondType) maybeType_whatisthisfor

    Can.TUnit ->
      Can.TUnit

    Can.TLambda a b ->
      Can.TLambda a b


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToDiffableType :: Text -> (Map.Map ModuleName.Canonical Interface.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> DiffableType
unionToDiffableType typeName interfaces recursionMap tvarMap unionInterface params =
  let
    treat union =
      let
        newTvarMap = tvarMap <> zip (Can._u_vars union) params

        debug dtype =
          debugHaskell ("\n✴️ inserting for union " <> typeName) (newTvarMap, dtype)
            & snd

      in
      -- debug $
      Can._u_alts union
        -- Sort constructors by name, this allows our diff to be stable to ordering changes
        & List.sortOn
          -- Ctor N.Name Index.ZeroBased Int [Type]
          (\(Can.Ctor name _ _ _) -> name)
        -- For each constructor
        & fmap (\(Can.Ctor name index int params_) ->
          ( N.toText name
          , -- For each constructor type param
            params_
              -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
              & fmap (resolveTvars_ newTvarMap)
              & fmap (\resolvedParam -> canonicalToDiffableType interfaces recursionMap resolvedParam newTvarMap )
          )
        )
        & DCustom typeName
  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


-- A top level Alias definition i.e. `type alias ...`
aliasToDiffableType :: (Map.Map ModuleName.Canonical Interface.Interface) -> [(ModuleName.Canonical, N.Name)] -> [(N.Name, Can.Type)] -> Interface.Alias -> [Can.Type] -> DiffableType
aliasToDiffableType interfaces recursionMap tvarMap aliasInterface params =
  let
    treat a =
      let
        debug dtype =
          debugHaskell ("\n🔵  inserting for alias") dtype
      in
      -- debug $

      case a of
        Can.Alias tvars tipe ->
          -- let
          --   !_ = formatHaskellValue "aliasToDiffableType" tvars :: IO ()
          -- in
          -- @TODO couldn't force an issue here but it seems wrong to ignore the tvars...
          -- why is aliasToDiffableType never called recursively from canonicalToDiffableType?
          -- it seems like it should be.
          canonicalToDiffableType interfaces recursionMap tipe tvarMap
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
canonicalToDiffableType :: Interfaces -> [(ModuleName.Canonical, N.Name)] -> Can.Type -> [(N.Name, Can.Type)] -> DiffableType
canonicalToDiffableType interfaces recursionMap canonical tvarMap =
  let
    debug dtype =
      debugHaskell ("\n✳️  inserting for type") dtype
      -- debugHaskellWhen (textContains "AnotherParamRecord" t) ("\n✳️  inserting def for " <> t <> " - " <> (T.pack . show $ canonical)) (t, imps, ft)
      -- debug_note ("🔵inserting def for " <> T.unpack t <> ":\n" <> ( ft)) $ (t, imps, ft)
      -- unsafePerformIO $ do
      --     formatHaskellValue ("\n🔵inserting def for " <> t) (ft) :: IO ()
      --     pure (t, imps, ft)
  in
  -- debug $

  case canonical of
    Can.TType moduleName name params ->
      let
        recursionIdentifier = (moduleName, name)

        newRecursionMap = [recursionIdentifier] ++ recursionMap

        tvarResolvedParams =
          params & fmap (resolveTvars_ tvarMap)

        identifier :: (Text, Text, Text, Text)
        identifier =
          case (moduleName, name) of
            ((ModuleName.Canonical (Pkg.Name author pkg) module_), tipe) ->
              (utf8ToText author, utf8ToText pkg, nameToText module_, nameToText tipe)

        kernelError =
          case identifier of
            (author, pkg, module_, tipe) ->
              DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_
      in

      if (List.any ((==) recursionIdentifier) recursionMap) then
        DRecursion $ case (moduleName, name) of
          ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) module_), typename) ->
            utf8ToText pkg1 <> "/" <> utf8ToText pkg2 <> ":" <> nameToText module_ <> "." <> nameToText typename

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

        -- These are particularly problematic for FrontendMsg, it means you can't
        -- get an E.Value from a HTTP call that then you parse later (problem for RPC!)
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
                      aliasToDiffableType interfaces newRecursionMap tvarMap alias tvarResolvedParams
                        & addExternalWarning (author, pkg, module_, tipe)

                    Nothing ->
                      DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_ <> ". Please report this issue with your code!"

            Nothing ->
              -- let !_ = formatHaskellValue "interface modulenames" (Map.keys interfaces) :: IO ()
              -- in
              DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


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
          canonicalToDiffableType interfaces recursionMap ti tvarMap

        Nothing ->
          let
            !_ = formatHaskellValue "Can.Tvar not found:" name :: IO ()
          in
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
  textSha1 $ diffableTypeToText dtype


diffableTypeToText :: DiffableType -> Text
diffableTypeToText dtype =
  case dtype of
    -- DRecord [(Text, DiffableType)]
    DRecord fields ->
      fields
        & fmap (\(n, tipe) -> diffableTypeToText tipe)
        & T.intercalate ""
        & (\v -> "R["<> v <>"]")

    -- DCustom [(Text, [DiffableType])]
    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            params
              & fmap diffableTypeToText
              & T.intercalate ""
              & (\t -> "["<> t <> "]")
          )
        & T.intercalate ""
        & (\v -> "C["<> v <>"]")

    -- DString
    DString ->
      "S"

    -- DInt
    DInt ->
      "I"

    -- DFloat
    DFloat ->
      "F"

    -- DBool
    DBool ->
      "B"

    -- DOrder
    DOrder ->
      "Ord"

    -- DNever
    DNever ->
      "Never"

    -- DChar
    DChar ->
      "Ch"

    -- DMaybe DiffableType
    DMaybe tipe ->
      "M["<> diffableTypeToText tipe <>"]"

    -- DList DiffableType
    DList tipe ->
      "L["<> diffableTypeToText tipe <>"]"

    -- DArray DiffableType
    DArray tipe ->
      "A["<> diffableTypeToText tipe <>"]"

    -- DSet DiffableType
    DSet tipe ->
      "S["<> diffableTypeToText tipe <>"]"

    -- DResult DiffableType DiffableType
    DResult err result ->
      "Res["<> diffableTypeToText err <>","<> diffableTypeToText result <>"]"

    -- DDict DiffableType DiffableType
    DDict key value ->
      "D["<> diffableTypeToText key <>","<> diffableTypeToText value <>"]"

    -- DTuple DiffableType DiffableType
    DTuple first second ->
      "T["<> diffableTypeToText first <>","<> diffableTypeToText second <>"]"

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
