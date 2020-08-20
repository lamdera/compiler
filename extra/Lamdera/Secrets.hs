{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- @TODO rename to AppConfig or something
module Lamdera.Secrets where

-- Clean up these
import AST.Module.Name (Canonical(..))
import AST.Optimized
import qualified Elm.Compiler.Objects as Obj
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import qualified System.Environment as Env

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Name as N


import Lamdera
import qualified Lamdera.Project

-- Querying
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Reporting.Task as Task
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Reporting.Exit.Http as E
import qualified Data.ByteString.Lazy as LBS
import qualified Reporting.Task.Http as Http
import qualified Data.ByteString.Builder as BS
import System.FilePath ((</>))
import System.Exit (exitFailure)

-- Check + Errors
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress

import Lamdera.Http
import qualified Lamdera.Login

writeUsage rootNames graph = do
  let
    prep usages =
      usages
        & Set.toList
        & fmap (\(module_, expression, secrets) ->
            E.list id
              [ E.text module_
              , E.text expression
              , secrets & fmap E.text & E.list id
              ]
          )
        & E.list id
        & E.encodeUgly
        & BS.toLazyByteString
        & LBS.toStrict
        & T.decodeUtf8

  case rootNames of
    [N.Name "Frontend"] -> do
      let usages = findSecretUses graph "Frontend" "app"
      -- progress $ hindentFormatValue usages
      liftIO $ writeUtf8Root "lamdera-stuff/.lamdera-fe-config" $ prep usages

    [N.Name "Backend"] -> do
      let usages = findSecretUses graph "Backend" "app"
      -- progress $ hindentFormatValue usages
      liftIO $ writeUtf8Root "lamdera-stuff/.lamdera-be-config" $ prep usages

    _ ->
      pure ()


progress t = do
  Task.report $ Progress.LamderaProgress $ D.stack [ D.fromText t, ""]


findSecretUses graph module_ expr = do
  let
    entryNode =
      Obj._nodes graph
         & Map.filterWithKey (\k a ->
             k == Global (Canonical (Pkg.Name "author" "project") (N.Name module_)) (N.Name expr)
         )

  if Map.null entryNode
    then Set.empty
    else entryNode & Map.elemAt 0 & traverse_ graph


onlyAuthorProjectDeps globalDeps =
  globalDeps
    & Set.filter (\global ->
      case global of
        Global (Canonical (Pkg.Name "author" "project") _) (N.Name expr) ->
          not (textContains "w2_" expr) && not (textContains "evg_" expr)
        _ ->
          False
    )

traverse_ :: Graph -> (Global, Node) -> Set.Set (Text, Text, [Text])
traverse_ graph (global, currentNode) = do
  let
    refs =
      nodeEnvRefs (global, currentNode)

  -- onlyWhen (not $ Set.null refs) $ do
    -- let
    (module_, expression) =
      case global of
        Global (Canonical (Pkg.Name "author" "project") (N.Name module_)) (N.Name expr) ->
          (module_, expr)

        _ ->
          error $ "Unexpected expression reduction. Please report this issue!\n\n" <> show global

    extractExpr global =
       case global of
         Global (Canonical (Pkg.Name "author" "project") _) (N.Name expr) ->
           expr

         _ ->
           error $ "Unexpected expression extraction. Please report this issue!\n\n" <> show global

    res = Set.singleton (module_, expression, refs & Set.map extractExpr & Set.toList)

    childResults =
      currentNode
        & selectNextDeps
        & Set.map (\global -> do
            Obj._nodes graph
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


nodeEnvRefs (global, node) =
  case node of
    Define expr globalDeps ->
      globalDeps
        & Set.filter (\globalDep ->
          case globalDep of
            Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) (N.Name expr) ->
              True

            _ ->
              False
        )

    _ ->
      Set.empty


readAppConfigUses :: Task.Task [(Text, Text, Text)]
readAppConfigUses =
  liftIO $ do
    root <- getProjectRoot
    feConfig <- readUtf8Text $ root </> "lamdera-stuff/.lamdera-fe-config"
    beConfig <- readUtf8Text $ root </> "lamdera-stuff/.lamdera-be-config"

    pure $ extract feConfig ++ extract beConfig


readAppFrontendConfigUses :: Task.Task [(Text, Text, Text)]
readAppFrontendConfigUses =
  liftIO $ do
    root <- getProjectRoot
    feConfig <- readUtf8Text $ root </> "lamdera-stuff/.lamdera-fe-config"

    pure $ extract feConfig


extract rawConfig = do
  let
    decoder =
      D.list $
        D.succeed (,,)
          & custom (D.index 0 D.text)
          & custom (D.index 1 D.text)
          & custom (D.index 2 (D.list D.text))

  case rawConfig of
    Just config -> do
      let bytes = T.encodeUtf8 config
      case D.parse "readAppConfigUses" id decoder bytes of
        Right values ->
          values
            & fmap (\(module_, expr, configs) ->
              configs & fmap (\config -> (module_, expr, config))
            )
            & concat

        Left jsonProblem -> do
          let !_ = debugHaskell "❌ config extraction failed" jsonProblem
          []
          -- return $ Left $ E.BadJson endpoint jsonProblem

    Nothing -> do
      let !_ = debugNote "❌ config read failed:" rawConfig
      []



-- Unused: attempt to find strings, non-exhaustive.
definedSecrets graph =
  -- DONE: find all defined secrets
  Obj._nodes graph
    & Map.filterWithKey (\k a ->
      case k of
        Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) _ ->
          -- True
          case a of
            Define (Str _) _ ->
              True

            _ ->
              False

        _ ->
          False
    )


exprContainsEnvString expr =
  case expr of
    VarGlobal (Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) _) ->
      True

    _ ->
      False


fetchAppConfigItems :: Text -> Text -> Task.Task (WithErrorField [(Text, Text, Bool, Bool)])
fetchAppConfigItems appName token = do
  let
    endpoint =
      if textContains "-local" appName
        then
          "http://localhost:8000/_r/configItemsJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://dashboard.lamdera.app/_r/configItemsJson"

    body =
      E.object
        [ ("key", E.text token)
        , ("appId", E.text appName)
        ]

    decoder =
      D.oneOf
        [ D.map SuccessField $ D.list $
            D.succeed (,,,)
              & required "name" D.text
              & required "value" D.text
              & required "used" D.bool
              & required "secret" D.bool
        , D.field "error" D.text
            & D.map ErrorField
        ]

  Lamdera.Http.normalRpcJson "fetchAppConfigItems" body endpoint decoder


checkUserConfig :: Text -> Maybe Text -> Task.Task ()
checkUserConfig appName prodTokenM = do

  token <-
    case prodTokenM of
      Just token ->
        pure token

      Nothing ->
        -- Will throw if invalid token forcing user to auth first
        liftIO $ Lamdera.Login.validateCliToken

  debug $ "Checking with token: " <> T.unpack token

  prodConfigItems <- do
    res <- Lamdera.Secrets.fetchAppConfigItems appName token
    case res of
      SuccessField configItems -> pure configItems
      ErrorField text ->
        Task.throw $ Exit.Lamdera
          $ Help.report "ERROR" Nothing
            ("A HTTP request failed with the following error:")
            [ D.reflow $ T.unpack text
            , D.reflow $ "Please check your configuration, or report this issue."
            ]


  localConfigItems <- Lamdera.Secrets.readAppConfigUses
  localFrontendConfigItems <- Lamdera.Secrets.readAppFrontendConfigUses

  -- Alert of any usages that don't have defined values
  let
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


  onlyWhen (length missingConfigs > 0) $ do
    let
      missingFormatted =
        missingConfigs
          & fmap (\(module_, expr, config) ->
              D.indent 4 $ D.fillSep [D.yellow $ D.fromText config , "in" , D.fromText $ module_ <> "." <> expr]
            )

    Task.throw $ Exit.Lamdera
      $ Help.report "MISSING PRODUCTION CONFIG" (Nothing)
        ("The following Env.elm config values are used but have no production value set:")
        ([ D.vcat missingFormatted
         , D.reflow "I need you to set a production value here before I can deploy:"
         , D.reflow $ "<https://dashboard.lamdera.app/app/" <> T.unpack appName <> ">"
         , D.reflow "See <https://dashboard.lamdera.app/docs/environment> more info."
         ]
        )

  -- Alert of any Frontend usages of secret config
  onlyWhen (length secretBreakingConfigs > 0) $ do
    let
      missingFormatted =
        secretBreakingConfigs
          & fmap (\(module_, expr, config) ->
              D.indent 4 $ D.fillSep [D.yellow $ D.fromText config , "in" , D.fromText $ module_ <> "." <> expr]
            )

    Task.throw $ Exit.Lamdera
      $ Help.report "MISSING PRODUCTION CONFIG" (Nothing)
        ("These secret config values are being exposed by frontend code!")
        ([ D.vcat missingFormatted
         , D.reflow "Remove these uses, or make config items public here:"
         , D.reflow $ "<https://dashboard.lamdera.app/app/" <> T.unpack appName <> ">"
         , D.reflow "See <https://dashboard.lamdera.app/docs/environment> more info."
         ]
        )


-- Production config value injection
injectConfig graph = do

  isTypeSnapshot <- Lamdera.isTypeSnapshot
  inProduction <- Lamdera.Project.inProduction

  if inProduction && not isTypeSnapshot
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

          token <-
            case prodTokenM of
              Just token ->
                pure token

              Nothing ->
                -- There must be a token present in production
                error "Error: could not generate production config, please report this."


          prodConfigItems <- do
            res_ <- Task.try Progress.silentReporter $ fetchAppConfigItems appName (T.pack token)
            case res_ of
              Just res ->
                case res of
                  SuccessField configItems -> pure configItems
                  ErrorField text -> do
                    pDocLn $ Help.reportToDoc $ Help.report "ERROR" Nothing
                        ("A HTTP request failed with the following error:")
                        [ D.red $ D.reflow $ T.unpack text
                        , D.reflow $ "Please check your configuration, or report this issue."
                        ]
                    exitFailure

              Nothing ->
                pure []

          let
            prodConfigMap =
              prodConfigItems
                & fmap (\v@(e1,e2,e3,e4) -> (e1, v) )
                & Map.fromList

          pure $
            graph { Obj._nodes =
              Obj._nodes graph
                & Map.mapWithKey (\k a ->
                  case k of
                    Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) (N.Name name_) ->
                      -- True
                      case a of
                        Define (Str t) gDeps ->
                          case Map.lookup name_ prodConfigMap of
                            Just (name, value, used, secret) -> do
                              let !_ = debugNote ("Injecting prod value for Env." <> name) value
                              -- Exists in prod, drop
                              Define (Str value) gDeps

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
