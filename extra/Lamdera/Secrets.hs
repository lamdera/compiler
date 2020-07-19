{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- @TODO rename to AppConfig or something
module Lamdera.Secrets where

-- Clean up these
import AST.Module.Name (Canonical(..))
import AST.Optimized (Global(..), Node(Define), Expr(..))
import qualified Elm.Compiler.Objects as Obj
import qualified Data.Set as Set

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Name as N

import Prelude hiding ((||), any)
import qualified Prelude

import Lamdera
import qualified Lamdera.Project

-- Querying
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Reporting.Task as Task
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Network.HTTP.Client as Client
-- import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified Network.HTTP.Types as Http
import qualified Reporting.Exit.Http as E
import qualified Data.ByteString.Lazy as LBS
import qualified Reporting.Task.Http as Http
import qualified Data.ByteString.Builder as BS
import System.FilePath ((</>))

-- Check + Errors
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress

writeUsage rootNames graph = do
  let
    usages =
      graph
        & Lamdera.Secrets.envNodes
        & fmap (\(module_, expression, secrets) -> (N.toText module_, N.toText expression, Set.map N.toText secrets))
        & fmap (\(module_, expression, secrets) ->
            E.list id
              [ E.text module_
              , E.text expression
              , Set.toList secrets & fmap E.text & E.list id
              ]
          )
        & E.list id
        & E.encodeUgly
        & BS.toLazyByteString
        & LBS.toStrict
        & T.decodeUtf8

    -- !_ = debugHaskell ("following exprs in " <> show_ rootNames <> " use Env.elm values") usages

  case rootNames of
    [N.Name "Frontend"] ->
      writeUtf8Root "lamdera-stuff/.lamdera-fe-config" usages

    [N.Name "Backend"] ->
      writeUtf8Root "lamdera-stuff/.lamdera-be-config" usages

    _ ->
      pure ()


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
      let !_ = debugNote "❌ config read failed" ""
      []



envNodes graph =
  -- fst $ Map.elemAt 1 ()
  -- works but not at the right stage, so still catches unused Env strings.


  -- Map.elemAt 1 $ Obj._nodes graph

  -- Debugging
  -- definedSecrets graph

  -- Obj._nodes graph
  --   & Map.filter nodeContainsEnvRef
    -- & Map.keys
    -- & length

  -- DONE: find all secret uses
  findSecretUses (Pkg.Name "author" "project") graph


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


nodeContainsEnvRef node =
  case node of
    Define expr globalDeps ->
      globalDeps
        & Set.filter (\global ->
          case global of
            Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) _ ->
              True

            _ ->
              False
        )
        & (not . Set.null)

    _ ->
      -- @TODO extend for all types
      False

      -- asdf =
      --   Env.blah
      --
      -- Results in ->
      --
      -- fromList
      --   [ ( Global
      --         (Canonical
      --            { _package = Name {_author = "author", _project = "project"}
      --            , _module = Name {_name = "Frontend"}
      --            })
      --         (Name {_name = "asdf"})
      --     , Define
      --         (VarGlobal
      --            (Global
      --               (Canonical
      --                  { _package = Name {_author = "author", _project = "project"}
      --                  , _module = Name {_name = "Env"}
      --                  })
      --               (Name {_name = "blah"})))
      --         (fromList
      --            [ Global
      --                (Canonical
      --                   { _package = Name {_author = "author", _project = "project"}
      --                   , _module = Name {_name = "Env"}
      --                   })
      --                (Name {_name = "blah"})
      --            ]))
      --   ]


exprContainsEnvString expr =
  case expr of
    VarGlobal (Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) _) ->
      True

    _ ->
      False


extractSecretName node =
  case node of
    Define (VarGlobal (Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) name)) _ ->
      name

    _ ->
      error "should be impossible"



-- FIND SECRET USES
-- Duplicate of FIND DEBUG USES from Nitpick


-- findSecretUses :: Pkg.Name -> Opt.Graph -> [(N.Name, N.Name)]
findSecretUses pkg (Opt.Graph _ graph _) =
  Set.toList $ Map.foldrWithKey (addSecretUses pkg) Set.empty graph


-- addSecretUses :: Pkg.Name -> Opt.Global -> Opt.Node -> Set.Set (N.Name, N.Name) -> Set.Set (N.Name, N.Name)
addSecretUses here (Opt.Global (ModuleName.Canonical pkg home) expr) node uses =
  let secrets = nodeHasSecret node
  in
  if pkg == here && not (Set.null secrets) then
    Set.insert (home, expr, secrets) uses
  else
    uses


nodeHasSecret :: Opt.Node -> Set.Set N.Name
nodeHasSecret node =
  case node of
    Opt.Define expr _           -> hasSecret expr
    Opt.DefineTailFunc _ expr _ -> hasSecret expr
    Opt.Ctor _ _                -> Set.empty
    Opt.Enum _                  -> Set.empty
    Opt.Box                     -> Set.empty
    Opt.Link _                  -> Set.empty
    Opt.Cycle _ vs fs _         -> any (hasSecret . snd) vs || any defHasSecret fs
    Opt.Manager _               -> Set.empty
    Opt.Kernel _ _              -> Set.empty
    Opt.PortIncoming expr _     -> hasSecret expr
    Opt.PortOutgoing expr _     -> hasSecret expr


hasSecret :: Opt.Expr -> Set.Set N.Name
hasSecret expression =
  case expression of
    Opt.Bool _           -> Set.empty
    Opt.Chr _            -> Set.empty
    Opt.Str _            -> Set.empty
    Opt.Int _            -> Set.empty
    Opt.Float _          -> Set.empty
    Opt.VarLocal _       -> Set.empty
    Opt.VarGlobal _      -> case expression of
                              VarGlobal (Global (Canonical (Pkg.Name "author" "project") (N.Name "Env")) expr) ->
                                Set.singleton expr
                              _ ->
                                Set.empty
    Opt.VarEnum _ _      -> Set.empty
    Opt.VarBox _         -> Set.empty
    Opt.VarCycle _ _     -> Set.empty
    Opt.VarDebug _ _ _ _ -> Set.empty
    Opt.VarKernel _ _    -> Set.empty
    Opt.List exprs       -> any hasSecret exprs
    Opt.Function _ expr  -> hasSecret expr
    Opt.Call e es        -> hasSecret e || any hasSecret es
    Opt.TailCall _ args  -> any (hasSecret . snd) args
    Opt.If conds finally -> any (\(c,e) -> hasSecret c || hasSecret e) conds || hasSecret finally
    Opt.Let def body     -> defHasSecret def || hasSecret body
    Opt.Destruct _ expr  -> hasSecret expr
    Opt.Case _ _ d jumps -> deciderHasSecret d || any (hasSecret . snd) jumps
    Opt.Accessor _       -> Set.empty
    Opt.Access r _       -> hasSecret r
    Opt.Update r fs      -> hasSecret r || any hasSecret fs
    Opt.Record fs        -> any hasSecret fs
    Opt.Unit             -> Set.empty
    Opt.Tuple a b c      -> hasSecret a || hasSecret b || maybe Set.empty hasSecret c
    Opt.Shader _ _ _     -> Set.empty


defHasSecret :: Opt.Def -> Set.Set N.Name
defHasSecret def =
  case def of
    Opt.Def _ expr       -> hasSecret expr
    Opt.TailDef _ _ expr -> hasSecret expr


deciderHasSecret :: Opt.Decider Opt.Choice -> Set.Set N.Name
deciderHasSecret decider =
  case decider of
    Opt.Leaf (Opt.Inline expr)  -> hasSecret expr
    Opt.Leaf (Opt.Jump _)       -> Set.empty
    Opt.Chain _ success failure -> deciderHasSecret success || deciderHasSecret failure
    Opt.FanOut _ tests fallback -> any (deciderHasSecret . snd) tests || deciderHasSecret fallback


(||) = Set.union

any fn things = foldl Set.union Set.empty (fmap fn things)



fetchAppConfigItems :: Text -> Bool -> Task.Task [(Text, Text, Bool, Bool)]
fetchAppConfigItems appName useLocal =
  let
    endpoint =
      if textContains "-local" appName && useLocal
        then
          "http://localhost:8000/_r/configItemsJson"
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
        else
          "https://" <> T.unpack appName <> ".lamdera.app/_r/configItemsJson"

    body =
      E.object
        -- @TODO this cannot be in source
        [ ("key", E.text "a739477eb8bd2acbc251c246438906f4")
        , ("appId", E.text appName)
        ]

    decoder =
      D.list $
        D.succeed (,,,)
          & required "name" D.text
          & required "value" D.text
          & required "used" D.bool
          & required "secret" D.bool
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      do  debug $ "HTTP POST " <> endpoint <> "\n---> " <> (
            body
              & E.encodeUgly
              & BS.toLazyByteString
              & LBS.toStrict
              & T.decodeUtf8
              & T.unpack
            )
          response <- httpPostJson manager request body

          let bytes = LBS.toStrict (Client.responseBody response)
          case D.parse endpoint id decoder bytes of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              return $ Left $ E.BadJson endpoint jsonProblem


required :: Text -> D.Decoder e a -> D.Decoder e (a -> b) -> D.Decoder e b
required key valDecoder decoder =
    custom (D.field key valDecoder) decoder


custom :: D.Decoder e a -> D.Decoder e (a -> b) -> D.Decoder e b
custom d1 d2 =
    D.map2 (\a_ fn_ -> fn_ a_) d1 d2


jsonHeaders =
  [ ( Http.hUserAgent, "lamdera-cli" )
  , ( Http.hContentType, "application/json" )
  , ( Http.hAccept, "application/json" )
  ]


httpPostJson manager request body =
  Client.httpLbs
    (request
      { Client.requestHeaders = jsonHeaders
      , Client.method = "POST"
      , Client.requestBody = Client.RequestBodyLBS $ BS.toLazyByteString $ E.encode body
      })
    manager


checkUserConfig appName = do
  -- @TODO skip when offline?

  prodConfigItems <- Lamdera.Secrets.fetchAppConfigItems appName True
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
         , D.reflow "See <https://dashboard.lamdera.app/docs/secrets> more info."
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
         , D.reflow "See <https://dashboard.lamdera.app/docs/secrets> more info."
         ]
        )


injectConfig graph = do

  inProduction <- Lamdera.Project.inProduction

  if inProduction
    then do

      appName <- Lamdera.Project.maybeAppName

      case appName of
        Nothing -> do
          debug "❌ injectConfig: No name set, skipping"
          -- No app name set, just return the graph as-is for now.
          pure $ graph

        Just appName -> do

          prodConfigItems <- Task.try Progress.silentReporter $ fetchAppConfigItems appName True

          let
            prodConfigMap =
              prodConfigItems
                & fromMaybe []
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
