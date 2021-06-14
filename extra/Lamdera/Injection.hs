{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Injection where

{- Additional injections into the Elm compiler JS output.
-}

import System.IO.Unsafe (unsafePerformIO)
import qualified File
import qualified System.Environment as Env
import qualified Data.ByteString.Builder as B
import Data.Monoid (mconcat)
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map as Map
import NeatInterpolation

import qualified Data.Name as Name
import qualified Generate.Mode as Mode
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified AST.Optimized as Opt
import qualified Elm.Kernel

import Lamdera
import StandaloneInstances

type Mains = Map.Map ModuleName.Canonical Opt.Main



graphModifications :: Mode.Mode -> Mains -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
graphModifications mode mains graph = do
  if mains & mainsInclude ["LocalDev"]
    then graph & Map.mapWithKey (modify $ isProdMode mode)
           -- & inspect
    else graph


modify :: Bool -> Opt.Global -> Opt.Node -> Opt.Node
modify isProd v n =
  case (v, n) of
    (Opt.Global (ModuleName.Canonical (Pkg.Name "elm" "kernel") "Http") name, Opt.Kernel chunks deps) ->
      let newChunks =
              chunks & fmap (\chunk ->
                case chunk of
                  Elm.Kernel.JS bs | bs & Text.decodeUtf8 & Text.isInfixOf "var _Http_toTask =" ->
                    bs
                      & Text.decodeUtf8
                      & Text.replace "var _Http_toTask =" "var _Http_toTask_REPLACED ="
                      & (<>) (modifiedHttp_toTask isProd)
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
modifiedHttp_toTask isProd =
  onlyIf (not isProd)
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

      window.cors_api_host = 'localhost:8001'

      var alterIfProxyRequired = function(url) {
        if (shouldProxy) {
          var cors_api_url = 'http://' + window.cors_api_host + '/'
          var origin = window.location.protocol + '//' + window.location.host
          var targetOrigin = /^https?:\/\/([^\/]+)/i.exec(url)
          if (targetOrigin && targetOrigin[0].toLowerCase() !== origin &&
              targetOrigin[1] !== window.cors_api_host) {
              url = cors_api_url + url;
          }
          return url
        } else {
          return url
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


source :: Mode.Mode -> Mains -> B.Builder
source mode mains =
  let
    isBackend = mains & mainsInclude ["Backend", "LBR"]
    isLocalDev = mains & mainsInclude ["LocalDev"]
  in
  B.byteString $ Text.encodeUtf8 $ injections isBackend isLocalDev


injections :: Bool -> Bool -> Text
injections isBackend isLocalDev =
  let
    isBackend_ =
      if isBackend
        then "true"
        else "false"

    exportFns =
      if isBackend
        then
          [text|
            var fns =
              { decodeWirePayloadHeader: $$author$$project$$LamderaHelpers$$decodeWirePayloadHeader
              , decodeWireAnalytics: $$author$$project$$LamderaHelpers$$decodeWireAnalytics
              }
          |]
        else
          [text|
            var fns = {}
          |]

    shouldProxy =
      onlyIf isLocalDev
        [text|
          shouldProxy = $$author$$project$$LocalDev$$shouldProxy(msg)
        |]
  in
  [text|

    var isBackend = $isBackend_ && typeof isLamdera !== 'undefined'

    function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
      {
        var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));

        // @TODO need to figure out how to get this to automatically escape by mode?
        //$$elm$$core$$Result$$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
        $$elm$$core$$Result$$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);

        var managers = {};
        var initPair = init(result.a);
        var model = (args && args['model']) || initPair.a;

        var stepper = stepperBuilder(sendToApp, model);
        var ports = _Platform_setupEffects(managers, sendToApp);

        var pos = 0;

        //console.log('managers', managers)
        //console.log('ports', ports)

        var dead = false;
        var upgradeMode = false;

        function mtime() {
          if (!isBackend) { return 0; }
          const hrTime = process.hrtime();
          return Math.floor(hrTime[0] * 1000000 + hrTime[1] / 1000);
        }

        function sendToApp(msg, viewMetadata)
        {
          if(dead){ return }
          if (upgradeMode) {
            // console.log('sendToApp.inactive',msg)
            // No more messages should run in upgrade mode
            // @TODO redirect messages somewhere
            _Platform_enqueueEffects(managers, $$elm$$core$$Platform$$Cmd$$none, $$elm$$core$$Platform$$Sub$$none);
            return;
          }
          //console.log('sendToApp.active',msg)

          $shouldProxy

          var start = mtime()
          var serializeDuration, logDuration = null

          start = mtime()
          var pair = A2(update, msg, model);

          const updateDuration = mtime() - start
          start = mtime()

          if (isBackend && loggingEnabled) {
            pos = pos + 1;
            const s = $$author$$project$$LBR$$serialize(msg);
            serializeDuration = mtime() - start
            start = mtime()
            insertEvent(pos, global.config.version, s.a, updateDuration, serializeDuration, A2($$elm$$core$$Maybe$$withDefault, null, s.b))
            logDuration = mtime() - start
          }

          // console.log(`model size: ${global.sizeof(pair.a)}`)
          // console.log(pair.a)

          stepper(model = pair.a, viewMetadata);
          //console.log('cmds', pair.b);
          _Platform_enqueueEffects(managers, pair.b, subscriptions(model));

          const stepEnqueueDuration = mtime() - start

          if (isBackend) {
            //console.log({serialize: serializeDuration, log: logDuration, update: updateDuration, stepEnqueue: stepEnqueueDuration})
          }
        }

        if ((args && args['model']) === undefined) {
          _Platform_enqueueEffects(managers, initPair.b, subscriptions(model));
        }

        $exportFns

        const die = function() {
          console.log('App dying')
          managers = null
          model = null
          stepper = null
          ports = null
        }

        return ports ? { ports: ports, gm: function() { return model }, eum: function() { upgradeMode = true }, die: die, fns: fns } : {};
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
elmPkgJs :: Mode.Mode -> B.Builder
elmPkgJs mode =
  case mode of
    Mode.Dev _ -> do
      unsafePerformIO $ do
        root <- getProjectRoot
        elmPkgJsSources <- safeListDirectory $ root </> "elm-pkg-js"

        wrappedPkgImports <-
          mapM
            (\f ->
              if ".js" `Text.isSuffixOf` (Text.pack f)
                then do
                  contents <- File.readUtf8 (root </> "elm-pkg-js" </> f)
                  pure $
                    "'" <> Text.encodeUtf8 (Text.pack f) <> "': function(exports){\n" <> contents <> "\nreturn exports;},\n"
                else
                  pure ""
            )
            elmPkgJsSources

        pure $ B.byteString $ mconcat
          [ "const pkgExports = {\n" <> mconcat wrappedPkgImports <> "\n}\n"
          , "if (typeof window !== 'undefined') {"
          , "  window.elmPkgJsInit = function(app) {"
          , "    for (var pkgId in pkgExports) {"
          , "      if (pkgExports.hasOwnProperty(pkgId)) {"
          , "        pkgExports[pkgId]({}).init(app)"
          , "      }"
          , "    }"
          , "  }"
          , "}"
          ]

    _ ->
      ""

onlyIf :: Bool -> Text -> Text
onlyIf cond text =
  if cond
    then text
    else ""


isProdMode :: Mode.Mode -> Bool
isProdMode mode =
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
