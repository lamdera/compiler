{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.ElmPages where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set

import qualified Data.Name as N
import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface
import qualified Reporting.Doc as D
import qualified Reporting.Exit

import StandaloneInstances

import Lamdera
import Lamdera.Wire3.Helpers
import Lamdera.Types (Interfaces)


data DiffableType
  = DRecord ModuleName.Canonical Text [(Text, DiffableType)]
  | DCustom Text [(Text, [DiffableType])]
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
  | DTriple DiffableType DiffableType DiffableType
  | DUnit
  | DRecursion Text
  | DKernelBrowser Text
  | DError Text
  -- Unused but kept for now to potentially merge with Lamdera's DiffableType in future
  | DExternalWarning (Text, Text, Text, Text) DiffableType
  deriving (Show)


checkPageDataType :: Interfaces -> Either Reporting.Exit.BuildProblem ()
checkPageDataType interfaces =
  case Map.lookup "Main" interfaces of
    Just targetInterface ->
      if typeExists "PageData" targetInterface
        then do
          let
            targetModule =
              (ModuleName.Canonical (Pkg.Name "author" "project") "Main")

            typediffs :: [(Text, DiffableType)]
            typediffs =
              let typediff = diffableTypeByName interfaces (toName "PageData") targetModule targetInterface
              in
              case typediff of
                DCustom name variants ->
                  variants & fmap (\(n, fs) -> fs & fmap (\f -> (n,f))) & List.concat
                _ ->
                  error $ "checkPageDataType: unexpected typeDiff for PageData. Please report this issue: " ++ show typediff

            errors :: [(Text, [Text], DiffableType)]
            errors =
              typediffs
                & fmap (\(t,tds) -> (t, diffableTypeErrors tds, tds))
                & filter (\(t,errs,tds) -> List.length errs > 0)

            formattedErrors :: [D.Doc]
            formattedErrors =
              errors
                & fmap (\(tipe, errors_, tds) ->
                    D.stack $
                      (errors_ & List.nub & fmap (\e -> D.fromChars . T.unpack $ T.concat ["- ", e]) )
                  )

          debug_note "Found Main.PageData, checking for wire constraints..." (Right ())

          if List.length errors > 0
            then
              wireError formattedErrors
            else do
              Right ()

        else do
          debug_note "Skipping elm-pages check, no Main.PageData found in interfaces" (Right ())
          Right ()

    Nothing -> do
      debug_note "Skipping elm-pages check, no Main found in interfaces" (Right ())
      Right ()


wireError formattedErrors =
  Left $
        Reporting.Exit.BuildLamderaProblem "WIRE ISSUES"
          "I found one or more Route Modules with Data types that contain functions."
          (formattedErrors ++
          [ D.reflow "See <https://dashboard.lamdera.app/docs/wire> for more info."
          ])


{- Tracks types that have already been seen to ensure we can break cycles -}
type RecursionSet =
  Set.Set (ModuleName.Raw, N.Name, [Can.Type])


nameRaw :: ModuleName.Canonical -> ModuleName.Raw
nameRaw (ModuleName.Canonical (Pkg.Name author pkg) module_) = module_


diffableTypeByName :: Interfaces -> N.Name -> ModuleName.Canonical -> Interface.Interface -> DiffableType
diffableTypeByName interfaces targetName modul interface = do
  let
    moduleName = ModuleName._module modul
    currentModule = modul
    recursionSet = Set.singleton (moduleName, targetName, [])

  case Map.lookup targetName $ Interface._aliases interface of
    Just alias -> do
      aliasToDiffableType targetName currentModule interfaces recursionSet [] alias []

    Nothing ->
      -- Try unions
      case Map.lookup targetName $ Interface._unions interface of
        Just union ->
          unionToDiffableType targetName currentModule (nameToText targetName) interfaces recursionSet [] union []

        Nothing ->
          DError $ T.concat ["Found no type named ", nameToText targetName, " in ", nameToText moduleName]


typeExists :: N.Name -> Interface.Interface -> Bool
typeExists targetName interface = do
  case Map.lookup targetName $ Interface._aliases interface of
    Just alias -> True
    Nothing ->
      case Map.lookup targetName $ Interface._unions interface of
        Just union -> True
        Nothing -> False


-- A top level Custom Type definition i.e. `type Herp = Derp ...`
unionToDiffableType :: N.Name -> ModuleName.Canonical -> Text -> Interfaces -> RecursionSet -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> DiffableType
unionToDiffableType targetName currentModule typeName interfaces recursionSet tvarMap unionInterface params =
  let
    treat union = unionConstructorsToDiffableTypes union targetName currentModule typeName interfaces recursionSet tvarMap unionInterface params
  in
  case unionInterface of
    Interface.OpenUnion u    -> DCustom typeName $ treat u
    Interface.ClosedUnion u  -> DCustom typeName $ treat u
    Interface.PrivateUnion u -> DCustom typeName $ treat u


unionConstructorsToDiffableTypes :: Can.Union -> N.Name -> ModuleName.Canonical -> Text -> Interfaces -> RecursionSet -> [(N.Name, Can.Type)] -> Interface.Union -> [Can.Type] -> [(Text, [DiffableType])]
unionConstructorsToDiffableTypes union targetName currentModule typeName interfaces recursionSet tvarMap unionInterface params =
  let
    newTvarMap = tvarMap <> zip (Can._u_vars union) params

    debug dtype =
      debugHaskell ("\n✴️ inserting for union " <> typeName) (newTvarMap, dtype)
        & snd
  in
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
          & fmap (resolveTvar newTvarMap)
          & fmap (\resolvedParam -> canonicalToDiffableType targetName currentModule interfaces recursionSet resolvedParam newTvarMap )
      )
    )


-- A top level Alias definition i.e. `type alias ...`
aliasToDiffableType :: N.Name -> ModuleName.Canonical -> Interfaces -> RecursionSet -> [(N.Name, Can.Type)] -> Interface.Alias -> [Can.Type] -> DiffableType
aliasToDiffableType targetName currentModule interfaces recursionSet tvarMap aliasInterface params =
  let
    treat a =
      case a of
        Can.Alias tvars tipe ->
          -- @TODO couldn't force an issue here but it seems wrong to ignore the tvars...
          -- why is aliasToDiffableType never called recursively from canonicalToDiffableType?
          -- it seems like it should be.
          canonicalToDiffableType targetName currentModule interfaces recursionSet tipe tvarMap
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
canonicalToDiffableType :: N.Name -> ModuleName.Canonical -> Interfaces -> RecursionSet -> Can.Type -> [(N.Name, Can.Type)] -> DiffableType
canonicalToDiffableType targetName currentModule interfaces recursionSet canonical tvarMap =
  case canonical of
    Can.TType moduleName name params ->
      let
        currentModule = moduleName

        recursionIdentifier = (nameRaw moduleName, name, tvarResolvedParams)

        newRecursionSet = Set.insert recursionIdentifier recursionSet

        tvarResolvedParams =
          params & fmap (resolveTvar tvarMap)

        identifier :: (Text, Text, Text, Text)
        identifier =
          case (moduleName, name) of
            ((ModuleName.Canonical (Pkg.Name author pkg) module_), tipe) ->
              (utf8ToText author, utf8ToText pkg, nameToText module_, nameToText tipe)

        kernelError =
          case identifier of
            (author, pkg, module_, tipe) ->
              DError $ T.concat ["must not contain kernel type `", tipe, "` from ", author, "/", pkg, ":", module_]

      in

      if (Set.member recursionIdentifier recursionSet) then
        DRecursion $ case (moduleName, name) of
          ((ModuleName.Canonical (Pkg.Name pkg1 pkg2) module_), typename) ->
            T.concat [utf8ToText pkg1, "/", utf8ToText pkg2, ":", nameToText module_, ".", nameToText typename]

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
              DMaybe (canonicalToDiffableType targetName currentModule interfaces recursionSet p tvarMap)

            _ ->
              DError "❗️impossible multi-param Maybe"

        ("elm", "core", "List", "List") ->
          case tvarResolvedParams of
            p:[] ->
              DList (canonicalToDiffableType targetName currentModule interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param List"

        ("elm", "core", "Array", "Array") ->
          case tvarResolvedParams of
            p:[] ->
              DArray (canonicalToDiffableType targetName currentModule interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param Array"

        ("elm", "core", "Set", "Set") ->
          case tvarResolvedParams of
            p:[] ->
              DSet (canonicalToDiffableType targetName currentModule interfaces recursionSet p tvarMap)
            _ ->
              DError "❗️impossible multi-param Set"

        ("elm", "core", "Result", "Result") ->
          case tvarResolvedParams of
            result:err:_ ->
              DResult (canonicalToDiffableType targetName currentModule interfaces recursionSet result tvarMap) (canonicalToDiffableType targetName currentModule interfaces recursionSet err tvarMap)
            _ ->
              DError "❗️impossible !2 param Result type"


        ("elm", "core", "Dict", "Dict") ->
          case tvarResolvedParams of
            result:err:_ ->
              DDict (canonicalToDiffableType targetName currentModule interfaces recursionSet result tvarMap) (canonicalToDiffableType targetName currentModule interfaces recursionSet err tvarMap)
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
        ("elm", "file", "File", "File") -> kernelError

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
                  unionToDiffableType targetName currentModule (N.toText name) interfaces newRecursionSet tvarMap union tvarResolvedParams

                Nothing ->
                  -- Try aliases
                  case Map.lookup name $ Interface._aliases subInterface of
                    Just alias -> do
                      aliasToDiffableType targetName currentModule interfaces newRecursionSet tvarMap alias tvarResolvedParams

                    Nothing ->
                      DError $ T.concat ["❗️Failed to find either alias or custom type for type that seemingly must exist: ", tipe, "` from ", author, "/", pkg, ":", module_, ". Please report this issue with your code!"]

            Nothing ->
              DError $ T.concat ["The `", tipe, "` type from ", author, "/", pkg, ":", module_, " is referenced, but I can't find it! You can try `lamdera install ", author, "/", pkg, "`, otherwise this might be a type which has been intentionally hidden by the author, so it cannot be used!"]


    Can.TAlias moduleName name tvarMap_ aliasType ->
      let
        addRecordName n t =
          case t of
            DRecord currentModule _ fields -> DRecord currentModule (nameToText n) fields
            _ -> t
      in
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
          in
          canonicalToDiffableType targetName moduleName interfaces recursionSet cType tvarResolvedMap
            & addRecordName name

        Can.Filled cType ->
          -- @TODO hypothesis...
          -- If an alias is filled, then it can't have any open holes within it either?
          -- So we can take this opportunity to reset tvars to reduce likeliness of naming conflicts?
          canonicalToDiffableType targetName moduleName interfaces recursionSet cType []
            & addRecordName name

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
              (N.toText name, canonicalToDiffableType targetName currentModule interfaces recursionSet tipe tvarMap)
            )
            & DRecord currentModule "unknown"

    Can.TTuple t1 t2 (Just t3) ->
      DTriple
        (canonicalToDiffableType targetName currentModule interfaces recursionSet t1 tvarMap)
        (canonicalToDiffableType targetName currentModule interfaces recursionSet t2 tvarMap)
        (canonicalToDiffableType targetName currentModule interfaces recursionSet t3 tvarMap)

    Can.TTuple t1 t2 _ ->
      DTuple
        (canonicalToDiffableType targetName currentModule interfaces recursionSet t1 tvarMap)
        (canonicalToDiffableType targetName currentModule interfaces recursionSet t2 tvarMap)

    Can.TUnit ->
      DUnit

    Can.TVar name ->
      -- Swap any `TVar (Name {_name = "a"})` for the actual injected params
      case List.find (\(t,ti) -> t == name) tvarMap of
        Just (_,ti) ->
          canonicalToDiffableType targetName currentModule interfaces recursionSet ti tvarMap

        Nothing ->
          let
            !_ = formatHaskellValue "Can.Tvar not found:" name :: IO ()
          in
          DError $ T.concat
            [ "Error: tvar lookup failed, please report this issue: cannot find "
            , N.toText name
            , " in tvarMap "
            ,  (T.pack $ show tvarMap)
            ]

    Can.TLambda _ _ ->
      DError $ "must not contain functions"


diffableTypeErrors :: DiffableType -> [Text]
diffableTypeErrors dtype =
  case dtype of
    DRecord moduleName name fields ->
      fields
        & fmap (\(n, tipe) -> do
            let errors = diffableTypeErrors tipe
            case errors of
              [] -> []
              xs -> errors & fmap (\err -> T.concat [nameToText (ModuleName._module moduleName), ".", name, ".", n, " ", err ])
          )
        & List.concat

    DCustom name constructors ->
      constructors
        & fmap (\(n, params) ->
            params
              & fmap (\param -> do
                  let errors = diffableTypeErrors param
                  case errors of
                    [] -> []
                    xs -> errors & fmap (\err -> T.concat [n, " ", err])
                )
              & List.concat
          )
        & List.concat

    DString             -> []
    DInt                -> []
    DFloat              -> []
    DBool               -> []
    DOrder              -> []
    DNever              -> [] -- Never is undecodable, but it's also unencodable, so we can support it on the basis that no Never values will ever exist!
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
