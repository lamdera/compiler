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

import qualified Generate.Mode as Mode
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified AST.Optimized as Opt

import Lamdera


type Mains = Map.Map ModuleName.Canonical Opt.Main


source :: Mode.Mode -> Mains -> B.Builder
source mode mains =
  let
    isBackend =
      case mains & Map.toList of
        ((ModuleName.Canonical (Pkg.Name author pkg) modul),_):[] ->
          if modul `elem` ["Backend", "LBR"]
            then True
            else False
        _ ->
          False
  in
  B.byteString $
  Text.encodeUtf8 $
  Text.concat $
  case mode of
    Mode.Dev _ ->
      [injections isBackend, corsAnywhere]
      -- [injections isBackend]

    Mode.Prod _ ->
      [injections isBackend]


injections :: Bool -> Text
injections isBackend =
  let
    conditional =
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
  in
  [text|
    function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
      {
        var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));

        // @TODO need to figure out how to get this to automatically escape by mode?
        //$$elm$$core$$Result$$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
        $$elm$$core$$Result$$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);

        var managers = {};
        var initPair = init(result.a);
        var model = args['model'] || initPair.a;

        var stepper = stepperBuilder(sendToApp, model);
        var ports = _Platform_setupEffects(managers, sendToApp);

        var pos = 0;

        //console.log('managers', managers)
        //console.log('ports', ports)

        // @TODO LocalDev hardcoding needs to go
        var upgradeMode = false;

        function mtime() {
          if (typeof window !== 'undefined') { return 0; }
          const hrTime = process.hrtime();
          return Math.floor(hrTime[0] * 1000000 + hrTime[1] / 1000);
        }

        function sendToApp(msg, viewMetadata)
        {
          if (upgradeMode) {
            // console.log('sendToApp.inactive',msg)
            // No more messages should run in upgrade mode
            // @TODO redirect messages somewhere
            _Platform_enqueueEffects(managers, $$elm$$core$$Platform$$Cmd$$none, $$elm$$core$$Platform$$Sub$$none);
            return;
          }
          //console.log('sendToApp.active',msg)

          var start = mtime()
          var serializeDuration, logDuration = null

          start = mtime()
          var pair = A2(update, msg, model);

          const updateDuration = mtime() - start
          start = mtime()

          if (typeof window == 'undefined') {
            pos = pos + 1;
            const s = $$author$$project$$LBR$$serialize(msg);
            serializeDuration = mtime() - start
            start = mtime()
            insertLog(pos, global.config.version, s.a, new Date(), updateDuration, serializeDuration, A2($$elm$$core$$Maybe$$withDefault, null, s.b))
            logDuration = mtime() - start
          }

          // console.log(`model size: ${global.sizeof(pair.a)}`)
          // console.log(pair.a)

          stepper(model = pair.a, viewMetadata);
          //console.log('cmds', pair.b);
          _Platform_enqueueEffects(managers, pair.b, subscriptions(model));

          const stepEnqueueDuration = mtime() - start

          if (typeof window == 'undefined') {
            //console.log({serialize: serializeDuration, log: logDuration, update: updateDuration, stepEnqueue: stepEnqueueDuration})
          }
        }

        if (args['model'] === undefined) {
          _Platform_enqueueEffects(managers, initPair.b, subscriptions(model));
        }

        $conditional

        return ports ? { ports: ports, gm: function() { return model }, eum: function() { upgradeMode = true }, fns: fns } : {};
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


{- Approach taken from cors-anywhere:
https://github.com/Rob--W/cors-anywhere/blame/master/README.md#L56

Rewrites all XHR requests from {url} to http://localhost:8001/{url}

See extra/Lamdera/ReverseProxy.hs for the proxy itself
-}
corsAnywhere :: Text
corsAnywhere =
  [text|
    (function() {
      if (typeof window == 'undefined') { return 0; } // skip for node.js
      var cors_api_host = 'localhost:8001';
      var cors_api_url = 'http://' + cors_api_host + '/';
      var slice = [].slice;
      var origin = window.location.protocol + '//' + window.location.host;
      var open = XMLHttpRequest.prototype.open;
      XMLHttpRequest.prototype.open = function() {
        var args = slice.call(arguments);
        var targetOrigin = /^https?:\/\/([^\/]+)/i.exec(args[1]);
        if (targetOrigin && targetOrigin[0].toLowerCase() !== origin &&
            targetOrigin[1] !== cors_api_host) {
            args[1] = cors_api_url + args[1];
        }
        return open.apply(this, args);
      };
    })();
  |]
  -- & Text.encodeUtf8
  -- & B.byteString


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
