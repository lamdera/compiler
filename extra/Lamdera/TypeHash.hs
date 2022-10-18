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
import qualified Data.Set as Set

import AST.Canonical
import qualified AST.Source as Valid
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result
import qualified Reporting.Doc as D
import qualified Reporting.Task as Task
import qualified Reporting.Exit
import qualified Build

import Lamdera
import Lamdera.Types
import Lamdera.Progress
import Lamdera.Wire3.Helpers
import qualified Ext.Query.Interfaces as Interfaces
import StandaloneInstances


{- Attempt to load all interfaces for project in current directory and generate
type snapshots  -}
calculateAndWrite :: IO (Either Reporting.Exit.BuildProblem ([Text], [(Text, [Text], DiffableType)]))
calculateAndWrite = do
  debug "calculate and write hashes"
  res <- calculateLamderaHashes
  case res of
    Right (hashes, warnings) -> do
      root <- getProjectRoot "calculateAndWrite"
      writeUtf8 (lamderaHashesPath root) $ show_ hashes
      pure res
    Left err ->
      pure res


buildCheckHashes :: Build.Artifacts -> Task.Task Reporting.Exit.Reactor ([Text], [(Text, [Text], DiffableType)])
buildCheckHashes artifacts = do
  Task.eio Reporting.Exit.ReactorBadBuild $ do
    calculateLamderaHashes
    -- @TODO this guard isn't needed with the safe Map.lookup access now added downstream,
    -- however left this here as a reminder that we might want more intelligent treatment of
    -- hash checking in future when we have the memorycached daemon mode.
    -- let
    --   didBuildTypes =
    --     Build._modules artifacts
    --       & fmap (\m ->
    --           case m of
    --             Build.Fresh moduleNameRaw _ _ -> moduleNameRaw == "Types"
    --             Build.Cached moduleNameRaw _ _ -> moduleNameRaw == "Types"
    --         )
    --       & any ((==) True)
    -- if didBuildTypes
    --   then calculateLamderaHashes
    --   else pure $ Right ([], [])


{- Tracks types that have already been seen to ensure we can break cycles -}
type RecursionSet =
  Set.Set (ModuleName.Raw, N.Name, [Type])


lamderaCoreTypes :: [ModuleName.Raw]
lamderaCoreTypes =
  [ "FrontendModel"
  , "BackendModel"
  , "FrontendMsg"
  , "ToBackend"
  , "BackendMsg"
  , "ToFrontend"
  ]


calculateHashPair :: FilePath -> N.Name -> N.Name -> IO (Text, Text)
calculateHashPair path modulename typename = do
  interfaces <- Interfaces.all [ path ]
  case Map.lookup modulename interfaces of
    Just interfaceModule -> do
      let dt = diffableTypeByName interfaces typename modulename interfaceModule
      pure $ (diffableTypeToHash dt, diffableTypeToText dt)
    Nothing ->
      error $ "calculateHashPair: did not find " ++ show modulename ++ " in interfaces."


calculateLamderaHashes :: IO (Either Reporting.Exit.BuildProblem ([Text], [(Text, [Text], DiffableType)]))
calculateLamderaHashes = do
  debug $ "🤝  calculating lamdera hashes"
  interfaces <- Interfaces.all [ "src/Types.elm" ]
  inDebug <- Lamdera.isDebug
  case Map.lookup "Types" interfaces of
    Just iface_Types ->
      calculateLamderaHashes_ interfaces iface_Types inDebug

    Nothing ->
      pure $ Right $ ([],[])


calculateLamderaHashes_ :: Interfaces -> Interface.Interface -> Bool -> IO (Either Reporting.Exit.BuildProblem ([Text], [(Text, [Text], DiffableType)]))
calculateLamderaHashes_ interfaces iface_Types inDebug = do
  let
    typediffs :: [(Text, DiffableType)]
    typediffs =
      lamderaCoreTypes
        & fmap (\t -> (nameToText t, diffableTypeByName interfaces t "Types" iface_Types))


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

    formattedErrors :: [D.Doc]
    formattedErrors =
      errors
        & fmap (\(tipe, errors_, tds) ->
            D.stack $
              [D.fillSep [ D.yellow $ D.fromChars . T.unpack $ tipe <> ":" ]]
              ++
              (errors_ & List.nub & fmap (\e -> D.fromChars . T.unpack $ "- " <> e) )
          )

  debug "🕸  Generating type hashes..."

  if List.length errors > 0
    then
      let
        -- !x = onlyWhen inDebug $ formatHaskellValue "diffHasErrors:" typediffs :: IO ()

        notifyWarnings =
          if List.length warnings > 0 then
            [ D.reflow $ "Warning: also, a number of types outside Types.elm are referenced, see `lamdera check` for more info." ]
          else
            []
      in
      pure $ Left $
        Reporting.Exit.BuildLamderaProblem "WIRE ISSUES"
          "I ran into the following problems when checking Lamdera core types:"
          (formattedErrors ++
          [ D.reflow "See <https://dashboard.lamdera.app/docs/wire> for more info."
          ] ++ notifyWarnings)

    else do
      -- -- These external warnings no longer need to be written to disk, but
      -- -- we might find it useful to evaluate the scope of external types that
      -- -- users are using in their projects?
      -- root <- getProjectRoot
      --
      -- if (List.length warnings > 0)
      --   then do
      --     writeUtf8 (lamderaExternalWarningsPath root) $ textWarnings
      --   else
      --     remove (lamderaExternalWarningsPath root)
      pure $ Right (hashes, warnings)


diffableTypeByName :: Interfaces -> N.Name -> N.Name -> Interface.Interface -> DiffableType
diffableTypeByName interfaces targetName moduleName interface = do
  let
    recursionSet = Set.singleton (moduleName, targetName, [])

  case Map.lookup targetName $ Interface._aliases interface of
    Just alias -> do
      aliasToDiffableType targetName interfaces recursionSet [] alias []

    Nothing ->
      -- Try unions
      case Map.lookup targetName $ Interface._unions interface of
        Just union ->
          unionToDiffableType targetName (nameToText targetName) interfaces recursionSet [] union []

        Nothing ->
          DError $ "Found no type named " <> nameToText targetName <> " in " <> nameToText moduleName



-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToDiffableType :: N.Name -> Text -> Interfaces -> RecursionSet -> [(N.Name, Type)] -> Interface.Union -> [Type] -> DiffableType
unionToDiffableType targetName typeName interfaces recursionSet tvarMap unionInterface params =
  let
    treat union =
      let
        newTvarMap = tvarMap <> zip (_u_vars union) params

        debug dtype =
          debugHaskell ("\n✴️ inserting for union " <> typeName) (newTvarMap, dtype)
            & snd
      in
      _u_alts union
        -- Sort constructors by name, this allows our diff to be stable to ordering changes
        & List.sortOn
          -- Ctor N.Name Index.ZeroBased Int [Type]
          (\(Ctor name _ _ _) -> name)
        -- For each constructor
        & fmap (\(Ctor name index int params_) ->
          ( N.toText name
          , -- For each constructor type param
            params_
              -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
              & fmap (resolveTvars newTvarMap)
              & fmap (\resolvedParam -> canonicalToDiffableType targetName interfaces recursionSet resolvedParam newTvarMap )
          )
        )
        & DCustom typeName
  in
  case unionInterface of
    Interface.OpenUnion u -> treat u
    Interface.ClosedUnion u -> treat u
    Interface.PrivateUnion u -> treat u


-- A top level Alias definition i.e. `type alias ...`
aliasToDiffableType :: N.Name -> Interfaces -> RecursionSet -> [(N.Name, Type)] -> Interface.Alias -> [Type] -> DiffableType
aliasToDiffableType targetName interfaces recursionSet tvarMap aliasInterface params =
  let
    treat a =
      case a of
        Alias tvars tipe ->
          -- @TODO couldn't force an issue here but it seems wrong to ignore the tvars...
          -- why is aliasToDiffableType never called recursively from canonicalToDiffableType?
          -- it seems like it should be.
          canonicalToDiffableType targetName interfaces recursionSet tipe tvarMap
  in
  case aliasInterface of
    Interface.PublicAlias a -> treat a
    Interface.PrivateAlias a -> treat a

nameRaw (ModuleName.Canonical (Pkg.Name author pkg) module_) = module_

-- = TLambda Type Type
-- | TVar N.Name
-- | TType ModuleName.Canonical N.Name [Type]
-- | TRecord (Map.Map N.Name FieldType) (Maybe N.Name)
-- | TUnit
-- | TTuple Type Type (Maybe Type)
-- | TAlias ModuleName.Canonical N.Name [(N.Name, Type)] AliasType
canonicalToDiffableType :: N.Name -> Interfaces -> RecursionSet -> Type -> [(N.Name, Type)] -> DiffableType
canonicalToDiffableType targetName interfaces recursionSet canonical tvarMap =
  case canonical of
    TType moduleName name params ->
      let
        recursionIdentifier = (nameRaw moduleName, name, tvarResolvedParams)

        newRecursionSet = Set.insert recursionIdentifier recursionSet

        tvarResolvedParams =
          params & fmap (resolveTvars tvarMap)

        identifier :: (Text, Text, Text, Text)
        identifier =
          case (moduleName, name) of
            ((ModuleName.Canonical (Pkg.Name author pkg) module_), tipe) ->
              (utf8ToText author, utf8ToText pkg, nameToText module_, nameToText tipe)

        kernelError =
          case identifier of
            (author, pkg, module_, tipe) ->
              DError $ "must not contain kernel type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_

        kernelErrorBrowserOnly =
          case identifier of
            (author, pkg, module_, tipe) ->
              DError $ "must not contain the Frontent-only type `" <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_
      in

      if (Set.member recursionIdentifier recursionSet) then
        DRecursion $ case (moduleName, name) of
          ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) module_), typename) ->
            utf8ToText pkg1 <> "/" <> utf8ToText pkg2 <> ":" <> nameToText module_ <> "." <> nameToText typename

      else
      case identifier of
        ("elm", "core", "String", "String") -> DString
        ("elm", "core", "Basics", "Int")    -> DInt
        ("elm", "core", "Basics", "Float")  -> DFloat
        ("elm", "core", "Basics", "Bool")   -> DBool
        ("elm", "core", "Basics", "Order")  -> DOrder
        ("elm", "core", "Basics", "Never")  -> DNever
        ("elm", "core", "Char", "Char")     -> DChar

        ("elm", "core", "Maybe", "Maybe") ->
          case tvarResolvedParams of
            p:[] ->
              DMaybe (canonicalToDiffableType targetName interfaces recursionSet p tvarMap)

            _ ->
              DError "❗️impossible multi-param Maybe"

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              DList (canonicalToDiffableType targetName interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param List"

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              DArray (canonicalToDiffableType targetName interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param Array"

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              DSet (canonicalToDiffableType targetName interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param Set"

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              DResult (canonicalToDiffableType targetName interfaces recursionSet result tvarMap) (canonicalToDiffableType targetName interfaces recursionSet err tvarMap)
            _ ->
              DError "❗️impossible !2 param Result type"


        ("elm", "core", "Dict", "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              DDict (canonicalToDiffableType targetName interfaces recursionSet result tvarMap) (canonicalToDiffableType targetName interfaces recursionSet err tvarMap)
            _ ->
              DError "❗️impossible !2 param Dict type"


        -- Values backed by JS Kernel types we cannot encode/decode
        ("elm", "virtual-dom", "VirtualDom", "Node")         -> kernelError
        ("elm", "virtual-dom", "VirtualDom", "Attribute")    -> kernelError
        ("elm", "virtual-dom", "VirtualDom", "Handler")      -> kernelError
        ("elm", "core", "Process", "Id")                     -> kernelError
        ("elm", "core", "Platform", "ProcessId")             -> kernelError
        ("elm", "core", "Platform", "Program")               -> kernelError
        ("elm", "core", "Platform", "Router")                -> kernelError
        ("elm", "core", "Platform", "Task")                  -> kernelError
        ("elm", "core", "Task", "Task")                      -> kernelError
        ("elm", "core", "Platform.Cmd", "Cmd")               -> kernelError
        ("elm", "core", "Platform.Sub", "Sub")               -> kernelError
        ("elm", "json", "Json.Decode", "Decoder")            -> kernelError
        -- These are particularly problematic for FrontendMsg, it means you can't
        -- get an E.Value from a HTTP call that then you parse later (problem for RPC!)
        ("elm", "json", "Json.Decode", "Value")              -> kernelError
        ("elm", "json", "Json.Encode", "Value")              -> kernelError
        ("elm", "http", "Http", "Body")                      -> kernelError
        ("elm", "http", "Http", "Part")                      -> kernelError
        ("elm", "http", "Http", "Expect")                    -> kernelError
        ("elm", "http", "Http", "Resolver")                  -> kernelError
        ("elm", "parser", "Parser", "Parser")                -> kernelError
        ("elm", "parser", "Parser.Advanced", "Parser")       -> kernelError
        ("elm", "regex", "Regex", "Regex")                   -> kernelError
        -- Not Kernel, but have functions... should we have them here?
        -- @TODO remove once we add test for functions in custom types
        ("elm", "url", "Url.Parser", "Parser")               -> kernelError
        ("elm", "url", "Url.Parser.Internal", "QueryParser") -> kernelError


        -- Frontend JS Kernel types
        ("elm", "file", "File", "File") ->
          if targetName `elem` ["FrontendMsg", "FrontendModel"] then
            DKernelBrowser "File.File"
          else
            kernelErrorBrowserOnly


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

          case Map.lookup (nameRaw moduleName) interfaces of
            Just subInterface ->

              -- Try unions
              case Map.lookup name $ Interface._unions subInterface of
                Just union -> do
                  unionToDiffableType targetName (N.toText name) interfaces newRecursionSet tvarMap union tvarResolvedParams
                    & addExternalWarning (author, pkg, module_, tipe)

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToDiffableType targetName interfaces newRecursionSet tvarMap alias tvarResolvedParams
                        & addExternalWarning (author, pkg, module_, tipe)

                    Nothing ->
                      DError $ "❗️Failed to find either alias or custom type for type that seemingly must exist: " <> tipe <> "` from " <> author <> "/" <> pkg <> ":" <> module_ <> ". Please report this issue with your code!"

            Nothing ->
              DError $ "The `" <> tipe <> "` type from " <> author <> "/" <> pkg <> ":" <> module_ <> " is referenced, but I can't find it! You can try `lamdera install " <> author <> "/" <> pkg <> "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"


    TAlias moduleName name tvarMap_ aliasType ->
      case aliasType of
        Holey cType ->
          let
            tvarResolvedMap =
              tvarMap_
                & fmap (\(n,p) ->
                  case p of
                    TVar a ->
                      case List.find (\(t,ti) -> t == a) tvarMap of
                        Just (_,ti) -> (n,ti)
                        Nothing -> (n,p)
                    _ -> (n,p)
                )
          in
          canonicalToDiffableType targetName interfaces recursionSet cType tvarResolvedMap

        Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
          canonicalToDiffableType targetName interfaces recursionSet cType []

    TRecord fieldMap extensibleName ->
      resolvedRecordFieldMap fieldMap extensibleName tvarMap
        & Map.toList
        & List.sortOn (\(name, field) -> name)
        & fmap (\(name,(FieldType index tipe)) ->
          (N.toText name, canonicalToDiffableType targetName interfaces recursionSet tipe tvarMap)
        )
        & DRecord

    TTuple firstType secondType maybeType_whatisthisfor ->
      DTuple (canonicalToDiffableType targetName interfaces recursionSet firstType tvarMap) (canonicalToDiffableType targetName interfaces recursionSet secondType tvarMap)

    TUnit ->
      DUnit

    TVar name ->
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          canonicalToDiffableType targetName interfaces recursionSet ti tvarMap

        Nothing ->
          let
            !_ = formatHaskellValue "Tvar not found:" name :: IO ()
          in
          DError $ "Error: tvar lookup failed, please report this issue: cannot find "
            <> N.toText name
            <> " in tvarMap "
            <>  (T.pack $ show tvarMap)

    TLambda t1 t2 ->
      DError $ "must not contain functions: " <> show_ t1 <> "\n" <> show_ t2


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
    DRecord fields ->
      fields
        & fmap (\(n, tipe) -> diffableTypeToText tipe)
        & T.intercalate ""
        & (\v -> "R["<> v <>"]")

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

    DString              -> "S"
    DInt                 -> "I"
    DFloat               -> "F"
    DBool                -> "B"
    DOrder               -> "Ord"
    DNever               -> "Never"
    DChar                -> "Ch"
    DMaybe tipe          -> "M["<> diffableTypeToText tipe <>"]"
    DList tipe           -> "L["<> diffableTypeToText tipe <>"]"
    DArray tipe          -> "A["<> diffableTypeToText tipe <>"]"
    DSet tipe            -> "S["<> diffableTypeToText tipe <>"]"
    DResult err result   -> "Res["<> diffableTypeToText err <>","<> diffableTypeToText result <>"]"
    DDict key value      -> "D["<> diffableTypeToText key <>","<> diffableTypeToText value <>"]"
    DTuple t1 t2         -> "T["<> diffableTypeToText t1 <>","<> diffableTypeToText t2 <>"]"
    DTriple t1 t2 t3     -> "T["<> diffableTypeToText t1 <>","<> diffableTypeToText t2 <>","<> diffableTypeToText t3 <>"]"
    DUnit                -> "()"

    DRecursion name ->
      -- Ideally recursion should be stable to name changes, but right now we
      -- have no easy way of knowing here if a name change coincided with a type
      -- change also, so for now name changes to recursive values count as "changed"
      "Recursion[" <> name <> "]"

    DKernelBrowser name ->
      "KB[" <> name <> "]"

    DError err -> "[ERROR]"
    DExternalWarning _ tipe -> diffableTypeToText tipe


diffableTypeErrors :: DiffableType -> [Text]
diffableTypeErrors dtype =
  case dtype of
    DRecord fields ->
      fields
        & fmap (\(n, tipe) -> diffableTypeErrors tipe)
        & List.concat

    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            fmap diffableTypeErrors params
              & List.concat
          )
        & List.concat

    DString             -> []
    DInt                -> []
    DFloat              -> []
    DBool               -> []
    DOrder              -> []
    DNever              -> ["must not contain Never values"]
    DChar               -> []
    DMaybe tipe         -> diffableTypeErrors tipe
    DList tipe          -> diffableTypeErrors tipe
    DArray tipe         -> diffableTypeErrors tipe
    DSet tipe           -> diffableTypeErrors tipe
    DResult err result  -> diffableTypeErrors err ++ diffableTypeErrors result
    DDict key value     -> diffableTypeErrors key ++ diffableTypeErrors value
    DTuple t1 t2        -> diffableTypeErrors t1 ++ diffableTypeErrors t2
    DTriple t1 t2 t3    -> diffableTypeErrors t1 ++ diffableTypeErrors t2 ++ diffableTypeErrors t3
    DUnit               -> []
    DRecursion name     -> []
    DKernelBrowser name -> []
    DError error        -> [error]

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

    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            fmap diffableTypeExternalWarnings params
              & List.concat
          )
        & List.concat

    DString             -> []
    DInt                -> []
    DFloat              -> []
    DBool               -> []
    DOrder              -> []
    DNever              -> []
    DChar               -> []
    DMaybe tipe         -> diffableTypeExternalWarnings tipe
    DList tipe          -> diffableTypeExternalWarnings tipe
    DArray tipe         -> diffableTypeExternalWarnings tipe
    DSet tipe           -> diffableTypeExternalWarnings tipe
    DResult err result  -> diffableTypeExternalWarnings err ++ diffableTypeExternalWarnings result
    DDict key value     -> diffableTypeExternalWarnings key ++ diffableTypeExternalWarnings value
    DTuple t1 t2        -> diffableTypeExternalWarnings t1 ++ diffableTypeExternalWarnings t2
    DTriple t1 t2 t3    -> diffableTypeExternalWarnings t1 ++ diffableTypeExternalWarnings t2 ++ diffableTypeExternalWarnings t3
    DUnit               -> []
    DRecursion name     -> []
    DKernelBrowser name -> []
    DError err          -> []

    DExternalWarning (author, pkg, module_, tipe) realtipe ->
      [ module_ <> "." <> tipe <> " (" <> author <> "/" <> pkg <> ")"]
        ++ diffableTypeExternalWarnings realtipe
