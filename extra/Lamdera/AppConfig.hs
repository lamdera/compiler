{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.AppConfig where

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Environment as Env
import System.FilePath ((</>))
import System.Exit (exitFailure)

import AST.Optimized
import Elm.ModuleName (Canonical(..))
import qualified Data.Name
import qualified Data.Utf8 as Utf8
import qualified Elm.Package as Pkg
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Reporting.Doc as D
import qualified Reporting.Exit
import qualified Reporting.Exit.Help as Help

import Lamdera
import Lamdera.Http
import Lamdera.Progress (throw, progress)
import qualified Lamdera.Graph
import qualified Lamdera.CLI.Login
import qualified Lamdera.Project
import qualified Ext.Query.Interfaces as Interfaces
import StandaloneInstances


writeUsage :: IO ()
writeUsage = do
  let
    prep usages =
      usages
        & Set.toList
        & fmap (\(module_, expression, secrets) ->
            E.list id
              [ E.string $ Utf8.fromChars $ T.unpack module_
              , E.string $ Utf8.fromChars $ T.unpack expression
              , secrets & fmap (E.string . Utf8.fromChars . T.unpack) & E.list id
              ]
          )
        & E.list id
        & E.encodeUgly
        & BS.toLazyByteString
        & LBS.toStrict
        & T.decodeUtf8

  cache <- lamderaCache_
  graph <- loadLamderaAppGraph

  findSecretUses graph "Frontend" "app"
    & prep
    & writeUtf8 (cache </> ".lamdera-fe-config")

  findSecretUses graph "Backend" "app"
    & prep
    & writeUtf8 (cache </> ".lamdera-be-config")


loadLamderaAppGraph :: IO GlobalGraph
loadLamderaAppGraph = do
  graph_ <- Lamdera.Graph.fullGraph ["src/Frontend.elm", "src/Backend.elm"]
  case graph_ of
    Left err ->
      throw $ Reporting.Exit.makeToReport err

    Right graph ->
      pure graph


findSecretUses :: GlobalGraph -> Data.Name.Name -> Data.Name.Name -> Set.Set (Text, Text, [Text])
findSecretUses graph module_ expr = do
  let
    entryNode =
      _g_nodes graph
         & Map.filterWithKey (\k a ->
             k == Global (Canonical (Pkg.Name "author" "project") (module_)) (expr)
         )

  if not $ Map.null entryNode
    then entryNode & Map.elemAt 0 & traverse_ graph
    else Set.empty


traverse_ :: GlobalGraph -> (Global, Node) -> Set.Set (Text, Text, [Text])
traverse_ graph (global, currentNode) = do
  let
    refs =
      nodeEnvRefs graph (global, currentNode)

  -- onlyWhen (not $ Set.null refs) $ do
    -- let
    (module_, expression) =
      case global of
        Global (Canonical (Pkg.Name "author" "project") (module_)) (expr) ->
          (module_, expr)

        _ ->
          error $ "Unexpected expression reduction. Please report this issue!\n\n" <> show global

    extractExpr global =
       case global of
         Global (Canonical (Pkg.Name "author" "project") _) expr ->
           nameToText expr

         _ ->
           error $ "Unexpected expression extraction. Please report this issue!\n\n" <> show global

    res :: Set.Set (Text, Text, [Text])
    res = Set.singleton (nameToText module_, nameToText expression, refs & Set.map extractExpr & Set.toList)

    childResults =
      currentNode
        & selectNextDeps
        & Set.map (\global -> do
            _g_nodes graph
               & Map.lookup global
               & (\nodeM ->
                case nodeM of
                  Just node -> traverse_ graph (global, node)
                  Nothing -> error "The impossible happened: an expression's global dep didn't exist in the graph. Please report this issue."
               )
          )
        & foldl Set.union Set.empty

  if Set.null refs
    then childResults
    else Set.union res childResults


selectNextDeps :: Node -> Set.Set Global
selectNextDeps node =
  case node of
    Define expr globalDeps ->
      globalDeps & onlyAuthorProjectDeps
    DefineTailFunc names expr globalDeps ->
      globalDeps & onlyAuthorProjectDeps
    Ctor index int ->
      Set.empty
    Enum index ->
      Set.empty
    Box ->
      Set.empty
    Link global ->
      Set.singleton global & onlyAuthorProjectDeps
    Cycle names namedExprs defs globalDeps ->
      -- Cycle [N.Name] [(N.Name, Expr)] [Def] GlobalDeps ->
      -- Need to confirm if we do indeed catch everything in a cycle
      -- already, but in any case this causes an infinite loop!
      Set.empty
    Manager effectsType ->
      Set.empty
    Kernel kContent kContentM ->
      Set.empty
    PortIncoming expr globalDeps ->
      globalDeps & onlyAuthorProjectDeps
    PortOutgoing expr globalDeps ->
      globalDeps & onlyAuthorProjectDeps


onlyAuthorProjectDeps :: Set.Set Global -> Set.Set Global
onlyAuthorProjectDeps globalDeps =
  globalDeps
    & Set.filter (\global ->
      case global of
        Global (Canonical (Pkg.Name "author" "project") _) name ->
          not (textHasPrefix "w2_" (nameToText name))
          && not (textHasPrefix "w3_" (nameToText name))
          && not (textHasPrefix "evg_" (nameToText name))
        _ ->
          False
    )


nodeEnvRefs :: GlobalGraph -> (a, Node) -> Set.Set Global
nodeEnvRefs graph (global, node) =
  case node of
    Define expr globalDeps ->
      globalDeps
        & Set.filter (\globalDep ->
          case globalDep of
            Global (Canonical (Pkg.Name "author" "project") "Env") expr ->
              True
            _ ->
              False
        )
        & Set.filter (\globalDep ->
          _g_nodes graph
             & Map.lookup globalDep
             & (\nodeM ->
              case nodeM of
                Just node ->
                  case node of
                    Define (Str _) _ ->
                      -- Only select string values defined in Env.elm
                      True

                    _ ->
                      False

                Nothing ->
                  -- Should not be possible
                  False
             )
        )

    _ ->
      Set.empty


readAppConfigUses :: IO [(Text, Text, Text)]
readAppConfigUses = do
  cache <- lamderaCache_
  feConfig <- readUtf8Text $ cache </> ".lamdera-fe-config"
  beConfig <- readUtf8Text $ cache </> ".lamdera-be-config"

  pure $ extract feConfig ++ extract beConfig


readAppFrontendConfigUses :: IO [(Text, Text, Text)]
readAppFrontendConfigUses = do
  cache <- lamderaCache_
  feConfig <- readUtf8Text $ cache </> ".lamdera-fe-config"

  pure $ extract feConfig


extract rawConfig = do
  let
    decoder =
      D.list $
        D.succeed (,,)
          & D.custom (D.index 0 D.text)
          & D.custom (D.index 1 D.text)
          & D.custom (D.index 2 (D.list D.text))

  case rawConfig of
    Just config -> do
      let bytes = T.encodeUtf8 config
      case D.fromByteString decoder bytes of
        Right values ->
          values
            & concatMap (\(module_, expr, configs) ->
              configs & fmap (\config -> (module_, expr, config))
            )

        Left problem -> do
          -- let !_ = debugHaskell "❌ config extraction failed" problem
          []
          -- return $ Left $ E.BadJson endpoint jsonProblem

    Nothing -> do
      let !_ = debugNote "❌ config read failed:" rawConfig
      []



fetchAppConfigItems :: Text -> Text -> IO (Either Lamdera.Http.Error (WithErrorField [(Text, Text, Bool, Bool)]))
fetchAppConfigItems appName token = do
  let
    endpoint =
      if textContains "-local" appName
        then
          "http://localhost:8082/_r/configItemsJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://dashboard.lamdera.app/_r/configItemsJson"

    body =
      E.object
        [ ("key", E.string $ Utf8.fromChars $ T.unpack token)
        , ("appId", E.string $ Utf8.fromChars $ T.unpack appName)
        ]

    decoder :: D.Decoder err (WithErrorField [(Text, Text, Bool, Bool)])
    decoder =
      D.oneOf
        [ fmap SuccessField $ D.list $
            D.succeed (,,,)
              & D.required "name" D.text
              & D.required "value" D.text
              & D.required "used" D.bool
              & D.required "secret" D.bool
        , D.field "error" D.text
            & fmap ErrorField
        ]

  Lamdera.Http.normalRpcJson "fetchAppConfigItems" body endpoint decoder


resultOrThrow :: Either Error (WithErrorField a) -> String -> IO a
resultOrThrow res_ reason =
  case res_ of
    Right res ->
      case res of
        SuccessField configItems ->
          pure configItems

        ErrorField text ->
          throwRequestFail $ T.unpack text

    Left err -> do
      Lamdera.Http.printHttpError err reason
      exitFailure


throwRequestFail :: String -> IO a
throwRequestFail text =
  throw $
    Help.report "ERROR" Nothing
      ("A HTTP request failed with the following error:")
      [ D.red $ D.reflow $ text
      , D.reflow $ "Please check your configuration, or report this issue."
      ]


checkUserConfig :: Text -> Maybe Text -> IO ()
checkUserConfig appName prodTokenM = do
  token <-
    case prodTokenM of
      Just token ->
        pure token

      Nothing ->
        -- Will throw if invalid token forcing user to auth first
        Lamdera.CLI.Login.validateCliToken

  debug $ "Checking with token: " <> T.unpack token

  prodConfigItems <- do
    res_ <- fetchAppConfigItems appName token
    resultOrThrow res_ "I needed to get production app config info"

  graph <- loadLamderaAppGraph

  let
    secretsNormalized secrets =
      secrets
        & Set.toList
        & concatMap (\(module_, expr, configs) ->
          configs & fmap (\config -> (module_, expr, config))
        )

    localFrontendConfigItems =
      findSecretUses graph "Frontend" "app"
        & secretsNormalized

    localConfigItems =
      findSecretUses graph "Backend" "app"
        & secretsNormalized
        & (++) localFrontendConfigItems

  -- @TODO old method writing to disk, needed anymore?
  -- localConfigItems <- readAppConfigUses
  -- localFrontendConfigItems <- readAppFrontendConfigUses

  -- Alert of any usages that don't have defined values
    prodConfigMap = prodConfigItems & fmap (\v@(e1,e2,e3,e4) -> (e1, v) ) & Map.fromList

    missingConfigs =
      localConfigItems
        & filter (\(module_, expression, config) ->
            case Map.lookup config prodConfigMap of
              Just x ->
                -- Exists in prod, drop
                False

              Nothing ->
                -- Missing in prod, keep
                True
        )
      & List.nub

    secretBreakingConfigs :: [(Text, Text, Text)]
    secretBreakingConfigs =
      localFrontendConfigItems
        & filter (\(module_, expression, config) ->
            case Map.lookup config prodConfigMap of
              Just (name_, value_, used_, secret) ->
                if secret then
                  -- Exists in prod and is secret, should not be usable
                  True
                else
                  -- Exists in prod and not secret, ignore
                  False

              Nothing ->
                -- Missing in prod, ignore
                False
        )

    formatSecrets secrets =
      secrets
        & fmap (\(module_, expr, config) ->
            D.indent 4 $ D.fillSep
              [ D.yellow $ D.fromChars $ T.unpack config
              , "in"
              , D.fromChars $ T.unpack module_ <> "." <> T.unpack expr]
          )
        & D.vcat

  onlyWhen (length missingConfigs > 0) $ do
    throw
      $ Help.report "MISSING PRODUCTION CONFIG" (Nothing)
        ("The following Env.elm config values are used but have no production value set:")
        ([ formatSecrets missingConfigs
         , D.reflow "I need you to set a production value here before I can deploy:"
         , D.reflow $ "<https://dashboard.lamdera.app/app/" <> T.unpack appName <> ">"
         , D.reflow "See <https://dashboard.lamdera.app/docs/environment> more info."
         ]
        )

  -- Alert of any Frontend usages of secret config
  onlyWhen (length secretBreakingConfigs > 0) $ do
    throw
      $ Help.report "EXPOSED PRODUCTION CONFIG" (Nothing)
        ("These secret config values are being exposed by frontend code!")
        ([ formatSecrets secretBreakingConfigs
         , D.reflow "Remove these uses, or make config items public here:"
         , D.reflow $ "<https://dashboard.lamdera.app/app/" <> T.unpack appName <> ">"
         , D.reflow "See <https://dashboard.lamdera.app/docs/environment> more info."
         ]
        )

  progress "Config items okay."


-- Production config value injection
injectConfig :: GlobalGraph -> IO GlobalGraph
injectConfig graph = do

  -- isTypeSnapshot <- Lamdera.isTypeSnapshot -- @TODO confirm this is right
  inProduction <- Lamdera.inProduction

  if inProduction -- && not isTypeSnapshot  -- @TODO confirm this is right
    then do
      debug "💉 Injecting production config"

      appName <- Lamdera.Project.maybeAppName

      case appName of
        Nothing -> do
          debug "❌ injectConfig: No name set, skipping"
          -- No app name set, just return the graph as-is for now.
          pure $ graph

        Just appName -> do

          prodTokenM <- Env.lookupEnv "TOKEN"

          !token <-
            case prodTokenM of
              Just token ->
                pure token

              Nothing ->
                -- There must be a token present in production
                error "Error: could not generate production config, please report this."

          prodConfigItems <- do
            res_ <- fetchAppConfigItems appName (T.pack token)
            resultOrThrow res_ "I needed to get production app config info"

          let
            prodConfigMap =
              prodConfigItems
                & fmap (\v@(e1,e2,e3,e4) -> (e1, v) )
                & Map.fromList

          pure $
            graph { _g_nodes =
              _g_nodes graph
                & Map.mapWithKey (\k a ->
                  case k of
                    Global (Canonical (Pkg.Name "author" "project") "Env") name_ ->
                      case a of
                        Define (Str t) gDeps ->
                          case Map.lookup (nameToText name_) prodConfigMap of
                            Just (name, value, used, secret) -> do
                              let !_ = debugNote ("Injecting prod value for Env." <> name) value
                              -- Exists in prod, drop
                              Define (Str (Utf8.fromChars . T.unpack $ value)) gDeps

                            Nothing -> do
                              let !_ = debugHaskell "impossible missing config item" (t, prodConfigMap)
                              -- Missing in prod, keep
                              -- Define (Str "MISSING-IMPOSSIBLE") gDeps
                              a

                        _ ->
                          a

                    _ ->
                      a
                )
            }
    else
      pure graph
