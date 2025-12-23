{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Injection where

{- Additional injections into the Elm compiler JS output.
-}

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Exit as Exit
import qualified File
import qualified System.Environment as Env
import qualified System.Directory as Dir
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)
import Data.Monoid (mconcat)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import NeatInterpolation

import qualified Data.Name as Name
import qualified Generate.Mode as Mode
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified AST.Optimized as Opt
import qualified Elm.Kernel

import Lamdera
import qualified Lamdera.Relative
import StandaloneInstances
import qualified Ext.Common

type Mains = Map.Map ModuleName.Canonical Opt.Main



graphModifications :: Mode.Mode -> Mains -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
graphModifications mode mains graph = do
  if mains & mainsInclude ["Lamdera.Live"]
    then graph & Map.mapWithKey (modify $ isOptimizedMode mode)
           -- & inspect
    else graph


modify :: Bool -> Opt.Global -> Opt.Node -> Opt.Node
modify isOptimized v n =
  case (v, n) of
    (Opt.Global (ModuleName.Canonical (Pkg.Name "elm" "kernel") "Http") name, Opt.Kernel chunks deps) ->
      let newChunks =
              chunks & fmap (\chunk ->
                case chunk of
                  Elm.Kernel.JS bs | bs & Text.decodeUtf8 & Text.isInfixOf "var _Http_toTask =" ->
                    bs
                      & Text.decodeUtf8
                      & Text.replace "var _Http_toTask =" "var _Http_toTask_REPLACED ="
                      & (<>) (modifiedHttp_toTask isOptimized)
                      & Text.encodeUtf8
                      & Elm.Kernel.JS
                  _ ->
                    chunk
              )
      in
      Opt.Kernel newChunks deps

    _ ->
      n


{- Approach taken from cors-anywhere:
https://github.com/Rob--W/cors-anywhere/blame/master/README.md#L56

Rewrites XHR request URLs from {url} to http://localhost:8001/{url}

Refined to use the shouldProxy global in context of the current msg type
being handled, so we can identify and proxy only BackendMsg task cmds,
leaving Frontend HTTP to still function as normal, including browser CORS
behaviors and limitations.

See extra/Lamdera/ReverseProxy.hs for the proxy itself
-}
modifiedHttp_toTask isOptimized =
  onlyIf (not isOptimized)
    -- Identical to original except for alterIfProxyRequired addition
    [text|

      var shouldProxy = false;

      var _Http_toTask = F3(function(router, toTask, request)
      {
        return _Scheduler_binding(function(callback)
        {
          function done(response) {
            callback(toTask(request.expect.a(response)));
          }

          var xhr = new XMLHttpRequest();
          xhr.addEventListener('error', function() { done($$elm$$http$$Http$$NetworkError_); });
          xhr.addEventListener('timeout', function() { done($$elm$$http$$Http$$Timeout_); });
          xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
          $$elm$$core$$Maybe$$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

          try {
            request.url = alterIfProxyRequired(request.url)
            xhr.open(request.method, request.url, true);
          } catch (e) {
            return done($$elm$$http$$Http$$BadUrl_(request.url));
          }

          _Http_configureRequest(xhr, request);

          request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
          xhr.send(request.body.b);

          return function() { xhr.c = true; xhr.abort(); };
        });
      });

      window.cors_api_host = 'localhost:8001';

      var alterIfProxyRequired = function(url) {
        if (shouldProxy) {
          var cors_api_url = 'http://' + window.cors_api_host + '/';
          var origin = window.location.protocol + '//' + window.location.host;
          var targetOrigin = /^https?:\/\/([^\/]+)/i.exec(url)
          if (targetOrigin && targetOrigin[0].toLowerCase() !== origin &&
              targetOrigin[1] !== window.cors_api_host) {
              url = cors_api_url + url;
          }
          return url;
        } else {
          return url;
        }
      }
    |]


inspect graph =
  let pick v n =
        case (v, n) of
          (Opt.Global (ModuleName.Canonical (Pkg.Name "elm" "kernel") "Http") name, Opt.Kernel _ _) ->
            True
          _ ->
            False
  in
  debugHaskellPass "graphModifications keys" (graph & Map.filterWithKey pick & Map.toList & take 5) graph


data OutputType = LamderaBackend | LamderaFrontend | LamderaLive | NotLamdera deriving (Eq)


outputType :: Mains -> OutputType
outputType mains
  | mains & mainsInclude ["Backend", "LBR"]  = LamderaBackend
  | mains & mainsInclude ["Frontend", "LFR"] = LamderaFrontend
  | mains & mainsInclude ["Lamdera.Live"]    = LamderaLive
  | otherwise                                = NotLamdera


source :: Mode.Mode -> Mains -> B.Builder
source mode mains =
  B.byteString $ Text.encodeUtf8 $ injections (outputType mains) mode


injections :: OutputType -> Mode.Mode -> Text
injections outputType mode =
  case outputType of
    NotLamdera ->
      [text|

    function _Lamdera_inject(app) {
      app.die = app.stop;
    }
      |]

    LamderaBackend ->
      [text|

    var isLamderaRuntime = typeof isLamdera !== 'undefined';

    function _Lamdera_inject(app, callUpdate, model) {
      app.die = app.stop;

      app.fns =
        { decodeWirePayloadHeader: $$author$$project$$LamderaHelpers$$decodeWirePayloadHeader
        , decodeWireAnalytics: $$author$$project$$LamderaHelpers$$decodeWireAnalytics
        , getUserModel : function() { return model.userModel }
        };

      var pos = 0;

      function mtime() { // microseconds
        if (!isLamderaRuntime) { return 0; }
        const hrTime = process.hrtime();
        return Math.floor(hrTime[0] * 1000000 + hrTime[1] / 1000);
      }

      callUpdate.call = function(update, msg, model) {
        var serializeDuration, logDuration = null;
        var start = mtime();

        var pair = A2(update, msg, model);

        const updateDuration = mtime() - start;
        start = mtime();

        if (isLamderaRuntime && loggingEnabled) {
          pos = pos + 1;
          const s = $$author$$project$$LBR$$serialize(msg);
          serializeDuration = mtime() - start;
          start = mtime();
          insertEvent(pos, global.config.version, s.a, updateDuration, serializeDuration, A2($$elm$$core$$Maybe$$withDefault, null, s.b));
          logDuration = mtime() - start;
        }

        return pair;
      };
    }
      |]

    LamderaFrontend ->
      [text|

    function _Lamdera_inject(app) {
      app.die = app.stop;
    }

    // In Elm, Browser.Navigation.Key is a function behind the scenes. It is passed and called here.
    // In Lamdera, the Key becomes an object after a Wire roundtrip, so we just take the key as a "password"
    // but then call the actual function ourselves. We _could_ Wire it as a reference, but then we have a
    // new problem: The key function is from the _old_ app and references functions and data from the old app.
    // So we would need to be able to update it, both so that it works and to not leak memory from the old app.
    // That could be doable by introducing a special reference Wire encoding specifically for Browser.Navigation.Key.
    // But even if we do that we still have a problem: Migrating from apps that doesn't have that new Wire encoding yet.
    // As long as we want to support migrating such apps, we have to stay with the solution below.
    var _Lamdera_navKey = function() {};
    var _Browser_go = F2(function(key, n) {
      return A2($$elm$$core$$Task$$perform, $$elm$$core$$Basics$$never, _Scheduler_binding(function() {
        n && history.go(n);
        _Lamdera_navKey();
      }));
    });
    // $$elm$$browser$$Browser$$Navigation$$back is not a direct assignment so it does not need to be replaced.
    var $$elm$$browser$$Browser$$Navigation$$forward = _Browser_go;
    var _Browser_pushUrl = F2(function(key, url) {
      return A2($$elm$$core$$Task$$perform, $$elm$$core$$Basics$$never, _Scheduler_binding(function() {
        history.pushState({}, "", url);
        _Lamdera_navKey();
      }));
    });
    var $$elm$$browser$$Browser$$Navigation$$pushUrl = _Browser_pushUrl;
    var _Browser_replaceUrl = F2(function(key, url) {
      return A2($$elm$$core$$Task$$perform, $$elm$$core$$Basics$$never, _Scheduler_binding(function() {
        history.replaceState({}, "", url);
        _Lamdera_navKey();
      }));
    });
    var $$elm$$browser$$Browser$$Navigation$$replaceUrl = _Browser_replaceUrl;
      |]

    LamderaLive ->
      [text|

    function _Lamdera_inject(app, callUpdate, model, sendToApp, args) {
      var errorHandler = args && args['errorHandler'];

      app.die = app.stop;
      
      app.fns =
        { getModel : function() { return model }
        , setBem : function(m) { model.bem = m; return m }
        , setFem : function(m) { model.fem = m; return m }
        , sendToApp : function(m) { sendToApp(m, true) }
        };

      callUpdate.call = function(update, msg, model) {
        shouldProxy = $$author$$project$$Lamdera$$Live$$shouldProxy(msg);
        if (errorHandler !== undefined) {
          return A2(update, msg, model);
        }
        try {
          return A2(update, msg, model);
        } catch (e) {
          errorHandler(e);
        }
      };
    }
      |]

  --   // https://github.com/elm/bytes/issues/20
  --   // but the fix below as suggested causes this problem:
  --   // https://github.com/nodejs/node/issues/26115
  --   _Bytes_read_string = F3(function (len, bytes, offset) {
  --     var decoder = new TextDecoder('utf8', { fatal:  true});
  --     var sliceView = new DataView(bytes.buffer, bytes.byteOffset + offset, len);
  --
  --     return _Utils_Tuple2(offset + len, decoder.decode(sliceView));
  --   });
  --
  -- |]

  -- var model = null
  -- window.addEventListener('bem', function (e) {
  --   model = e.detail
  -- }, false);
  -- window.dispatchEvent(new Event('rbem'));

  -- unsafePerformIO $ do
  --
  --   injectionsM <- Env.lookupEnv "BACKENDINJECTION"
  --
  --   Lamdera.debug_ $ "Got " <> show injectionsM <> " for BACKENDINJECTION"
  --
  --   case injectionsM of
  --     Just injectionsPath -> do
  --       Lamdera.debug_ $ "Injecting " <> injectionsPath <> " into final source"
  --       B.byteString <$> File.readUtf8 injectionsPath
  --
  --     Nothing ->
  --       -- No injections, so we'll inject empty string
  --       pure ""


{- elm-pkg-js integration
See: https://github.com/supermario/elm-pkg-js
-}
{-# NOINLINE elmPkgJs #-}
elmPkgJs :: Mode.Mode -> Mains -> B.Builder
elmPkgJs mode mains =
  case mode of
    Mode.Dev _ -> do
      unsafePerformIO $ do
        root <- getProjectRoot "elmPkgJs"
        elmPkgJsSources <- safeListDirectory $ root </> "elm-pkg-js"

        let
          precompiledElmPkgJs =
            case outputType mains of
              LamderaLive -> precompiledLamderaLiveElmPkgJs
              _           -> []

        includesPathM <- Lamdera.Relative.findFile $ root </> "elm-pkg-js-includes.js"
        esbuildConfigPathM <- Lamdera.Relative.findFile $ root </> "esbuild.config.js"
        esbuildPathM <- Dir.findExecutable "esbuild"

        case (esbuildConfigPathM, esbuildPathM, includesPathM) of
          (Just esbuildConfigPath, _, _) ->
            if Ext.Common.isDebug_
              then do
                Lamdera.debug_ "🏗️  Building esbuild.config.js"
                hasNode <- Dir.findExecutable "node"
                minFile <- case hasNode of
                  Just node -> do
                    Ext.Common.bash $ "cd " <> takeDirectory esbuildConfigPath <> " && " <> node <> " " <> esbuildConfigPath
                    Lamdera.Relative.readFile $ root </> "elm-pkg-js-includes.min.js"
                  Nothing ->
                    error "Could not find path to node"

                case minFile of
                  Just minFileContents -> do
                    pure $ Ext.Common.textToBuilder minFileContents
                  Nothing -> do
                    error "no min file after compile, run `node esbuild.config.js` to check errors"
              else do
                Lamdera.debug_ "🏗️🟠  Using dumbJsPackager, ignoring esbuild.config.js in non-dev mode"
                dumbJsPackager root elmPkgJsSources precompiledElmPkgJs
          (_, Just esbuildPath, Just includesPath) ->
            if Ext.Common.isDebug_
              then do
                esbuildIncluder root esbuildPath includesPath
              else do
                Lamdera.debug_ "🏗️🟠  Using dumbJsPackager, ignoring esbuild in non-dev mode"
                dumbJsPackager root elmPkgJsSources precompiledElmPkgJs
          _ -> do
            Lamdera.debug_ "🏗️  Using dumbJsPackager"
            dumbJsPackager root elmPkgJsSources precompiledElmPkgJs
    _ ->
      ""


esbuildIncluder :: FilePath -> FilePath -> FilePath -> IO B.Builder
esbuildIncluder root esbuildPath includesPath = do
  minFile <- Lamdera.Relative.readFile $ root </> "elm-pkg-js-includes.min.js"
  case minFile of
    Just minFileContents -> do
      Lamdera.debug_ "🏗️  Using cached elm-pkg-js-includes.min.js"
      pure $ Ext.Common.textToBuilder minFileContents
    Nothing -> do
      Lamdera.debug_ "🏗️  Building elm-pkg-js-includes.js with esbuild"
      -- packaged <- Ext.Common.cq_ esbuildPath [ includesPath, "--bundle", "--global-name=elmPkgJsIncludes" ] ""
      (exit, packaged, stdErr) <- Ext.Common.cq_ esbuildPath [ includesPath, "--bundle", "--minify", "--global-name=elmPkgJsIncludes" ] ""
      case exit of
        Exit.ExitFailure code -> do
          atomicPutStrLn stdErr
          pure "ESBUILD ERRORS SEE CONSOLE"

        Exit.ExitSuccess ->
          packaged
            & Ext.Common.stringToBuilder
            -- & debugHaskell "minified elmpkgjs"
            & pure

  -- Build individual files? Any point?
  -- elmPkgJsSources & mapM
  --   (\f ->
  --     if ".js" `Text.isSuffixOf` (Text.pack f) || ".ts" `Text.isSuffixOf` (Text.pack f)
  --       then do
  --         contents <- File.readUtf8 (root </> "elm-pkg-js" </> f)
  --         pure $
  --           "'" <> Text.encodeUtf8 (Text.pack f) <> "': function(exports){\n" <> contents <> "\nreturn exports;},\n"
  --       else
  --         pure ""
  --   )


precompiledLamderaLiveElmPkgJs :: [(String, BS.ByteString)]
precompiledLamderaLiveElmPkgJs =
  [ ("repl", $(bsToExp =<< runIO (Lamdera.Relative.readByteString "extra/elm-pkg-js/repl.js")))
  ]


mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = fmap catMaybes (traverse f xs)


-- Tries to be clever by injecting `{}` as the `exports` value. Falls over if the target files have been compiled
-- by a packager or if they don't use the `export.init` syntax, i.e. `export async function init() {...}`
dumbJsPackager root elmPkgJsSources precompiledElmPkgJs =
  if null elmPkgJsSources && null precompiledElmPkgJs
    then
      pure ""

    else do
      fileContents <-
        mapMaybeM
          (\f ->
            if ".js" `Text.isSuffixOf` Text.pack f
              then do
                contents <- File.readUtf8 (root </> "elm-pkg-js" </> f)
                pure $ Just (f, contents)
              else
                pure Nothing
          )
          elmPkgJsSources

      let
        wrappedPkgImports =
          [ "'" <> Text.encodeUtf8 (Text.pack filename) <> "': function(exports){\n" <> contents <> "\nreturn exports;},\n"
          | (filename, contents) <- fileContents ++ precompiledElmPkgJs
          ]

      pure $ B.byteString $ mconcat
        [ "const pkgExports = {\n" <> mconcat wrappedPkgImports <> "\n}\n"
        , "if (typeof window !== 'undefined') {"
        , "  window.elmPkgJsIncludes = {"
        , "    init: async function(app) {"
        , "      for (var pkgId in pkgExports) {"
        , "        if (pkgExports.hasOwnProperty(pkgId)) {"
        , "          pkgExports[pkgId]({}).init(app)"
        , "        }"
        , "      }"
        , "    }"
        , "  }"
        , "}"
        ]


onlyIf :: Bool -> Text -> Text
onlyIf cond t =
  if cond
    then t
    else ""


isOptimizedMode :: Mode.Mode -> Bool
isOptimizedMode mode =
    case mode of
      Mode.Dev _ -> False
      Mode.Prod _ -> True


mainsInclude :: [Name.Name] -> Mains -> Bool
mainsInclude list mains =
  case mains & Map.toList of
    ((ModuleName.Canonical (Pkg.Name author pkg) modul),_):[] ->
      if modul `elem` list
        then True
        else False
    _ ->
      False
