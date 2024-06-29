{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Injection where

{- Additional injections into the Elm compiler JS output.
-}

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Exit as Exit
import qualified File
import qualified System.Environment as Env
import qualified System.Directory as Dir
import qualified Data.ByteString.Builder as B
import Data.Monoid (mconcat)
import System.FilePath ((</>), takeDirectory)
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
import qualified Lamdera.Relative
import StandaloneInstances
import qualified Ext.Common

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


source :: Mode.Mode -> Mains -> B.Builder
source mode mains =
  let
    isBackend = mains & mainsInclude ["Backend", "LBR"]
    isLocalDev = mains & mainsInclude ["LocalDev"]
  in
  B.byteString $ Text.encodeUtf8 $ injections mode isBackend isLocalDev


injections :: Mode.Mode -> Bool -> Bool -> Text
injections mode isBackend isLocalDev =
  let
    previousVersionInt =
      -- @TODO maybe its time to consolidate the global config...
      (unsafePerformIO $ lookupEnv "VERSION")
        & maybe "0" id
        & read
        & subtract 1

    previousVersion = show_ previousVersionInt

    isBackend_ =
      if isBackend
        then "true"
        else "false"

    runUpdate =
      if isLocalDev
        then
          [text|
            var pair = A2(update, msg, model);
          |]
        else
          [text|
            try {
              var pair = A2(update, msg, model);
            } catch(err) {
              if (isBackend) { bugsnag.notify(err); }
              return;
            }
          |]

    exportFns =
      if isBackend
        then
          [text|
            var fns =
              { decodeWirePayloadHeader: $$author$$project$$LamderaHelpers$$decodeWirePayloadHeader
              , decodeWireAnalytics: $$author$$project$$LamderaHelpers$$decodeWireAnalytics
              , getUserModel : function() { return model.userModel }
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

    debugToAnsiStringOverride =
      [text|
        function _Debug_toAnsiString(ansi, value)
        {
          if (typeof value === 'function')
          {
            return _Debug_internalColor(ansi, '<function>');
          }

          if (typeof value === 'boolean')
          {
            return _Debug_ctorColor(ansi, value ? 'True' : 'False');
          }

          if (typeof value === 'number')
          {
            return _Debug_numberColor(ansi, value + '');
          }

          if (value instanceof String)
          {
            return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
          }

          if (typeof value === 'string')
          {
            return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
          }

          if (typeof value === 'object' && '$$' in value)
          {
            var tag = value.$$;

            if (typeof tag === 'number')
            {
              return _Debug_internalColor(ansi, '<internals>');
            }

            if (tag[0] === '#')
            {
              var output = [];
              for (var k in value)
              {
                if (k === '$$') continue;
                output.push(_Debug_toAnsiString(ansi, value[k]));
              }
              return '(' + output.join(',') + ')';
            }

            if (tag === 'Set_elm_builtin')
            {
              return _Debug_ctorColor(ansi, 'Set')
                + _Debug_fadeColor(ansi, '.fromList') + ' '
                + _Debug_toAnsiString(ansi, $$elm$$core$$Set$$toList(value));
            }

            if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
            {
              return _Debug_ctorColor(ansi, 'Dict')
                + _Debug_fadeColor(ansi, '.fromList') + ' '
                + _Debug_toAnsiString(ansi, $$elm$$core$$Dict$$toList(value));
            }

            if (tag === 'SeqSet_elm_builtin')
            {
              return _Debug_ctorColor(ansi, 'SeqSet')
                + _Debug_fadeColor(ansi, '.fromList') + ' '
                + _Debug_toAnsiString(ansi, $$lamdera$$containers$$SeqSet$$toList(value));
            }

            if (tag === 'SeqDict_elm_builtin')
            {
              return _Debug_ctorColor(ansi, 'SeqDict')
                + _Debug_fadeColor(ansi, '.fromList') + ' '
                + _Debug_toAnsiString(ansi, $$lamdera$$containers$$SeqDict$$toList(value));
            }

            if (tag === 'Array_elm_builtin')
            {
              return _Debug_ctorColor(ansi, 'Array')
                + _Debug_fadeColor(ansi, '.fromList') + ' '
                + _Debug_toAnsiString(ansi, $$elm$$core$$Array$$toList(value));
            }

            if (tag === '::' || tag === '[]')
            {
              var output = '[';

              value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

              for (; value.b; value = value.b) // WHILE_CONS
              {
                output += ',' + _Debug_toAnsiString(ansi, value.a);
              }
              return output + ']';
            }

            var output = '';
            for (var i in value)
            {
              if (i === '$$') continue;
              var str = _Debug_toAnsiString(ansi, value[i]);
              var c0 = str[0];
              var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
              output += ' ' + (parenless ? str : '(' + str + ')');
            }
            return _Debug_ctorColor(ansi, tag) + output;
          }

          if (typeof DataView === 'function' && value instanceof DataView)
          {
            return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
          }

          if (typeof File !== 'undefined' && value instanceof File)
          {
            return _Debug_internalColor(ansi, '<' + value.name + '>');
          }

          if (typeof value === 'object')
          {
            var output = [];
            for (var key in value)
            {
              var field = key[0] === '_' ? key.slice(1) : key;
              output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
            }
            if (output.length === 0)
            {
              return '{}';
            }
            return '{ ' + output.join(', ') + ' }';
          }

          return _Debug_internalColor(ansi, '<internals>');
        }
      |]

    equalsOverride =
        if isProdMode mode then
         [text|
              if (x.$$ < 0)
              {
                if (x.$$ < -10)
                {
                  x = $$lamdera$$containers$$SeqDict$$toList(x);
                  y = $$lamdera$$containers$$SeqDict$$toList(y);
                }
                else
                {
                  x = $$elm$$core$$Dict$$toList(x);
                  y = $$elm$$core$$Dict$$toList(y);
                }
              }
         |]
       else
         [text|
            if (x.$$ === 'Set_elm_builtin')
            {
              x = $$elm$$core$$Set$$toList(x);
              y = $$elm$$core$$Set$$toList(y);
            }
            if (x.$$ === 'RBNode_elm_builtin' || x.$$ === 'RBEmpty_elm_builtin')
            {
              x = $$elm$$core$$Dict$$toList(x);
              y = $$elm$$core$$Dict$$toList(y);
            }
            if (x.$$ === 'SeqDict_elm_builtin')
            {
              x = $$lamdera$$containers$$SeqDict$$toList(x);
              y = $$lamdera$$containers$$SeqDict$$toList(y);
            }
            if (x.$$ === 'SeqSet_elm_builtin')
            {
              x = $$lamdera$$containers$$SeqSet$$toList(x);
              y = $$lamdera$$containers$$SeqSet$$toList(y);
            }
         |]
  in
  [text|

    $debugToAnsiStringOverride

    function _Debug_addSlashes(str, isChar)
    {
      var s = str
        .replace(/\\/g, '\\\\')
        .replace(/\n/g, '\\n')
        .replace(/\t/g, '\\t')
        .replace(/\r/g, '\\r')
        .replace(/\v/g, '\\v')
        .replace(/\0/g, '\\0');

      if (isChar)
      {
        return s.replace(/\'/g, '\\\'');
      }
      else
      {
        return s.replace(/\"/g, '\\"');
      }
    }

    function _Utils_eqHelp(x, y, depth, stack)
    {
      if (x === y)
      {
        return true;
      }

      if (typeof x !== 'object' || x === null || y === null)
      {
        typeof x === 'function' && $$elm$$core$$Debug$$crash(5);
        return false;
      }

      if (depth > 100)
      {
        stack.push(_Utils_Tuple2(x,y));
        return true;
      }

      $equalsOverride

      for (var key in x)
      {
        if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
        {
          return false;
        }
      }
      return true;
    }

    var isBackend = $isBackend_ && typeof isLamdera !== 'undefined';

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

        function mtime() { // microseconds
          if (!isBackend) { return 0; }
          const hrTime = process.hrtime();
          return Math.floor(hrTime[0] * 1000000 + hrTime[1] / 1000);
        }

        function sendToApp(msg, viewMetadata)
        {
          if(dead){ return }
          if (upgradeMode) {
            // console.log('sendToApp.inactive',msg);
            // No more messages should run in upgrade mode
            // @TODO redirect messages somewhere
            _Platform_enqueueEffects(managers, $$elm$$core$$Platform$$Cmd$$none, $$elm$$core$$Platform$$Sub$$none);
            return;
          }
          //console.log('sendToApp.active',msg);

          $shouldProxy

          var serializeDuration, logDuration = null;
          var start = mtime();

          $runUpdate

          const updateDuration = mtime() - start;
          start = mtime();

          if (isBackend && loggingEnabled) {
            pos = pos + 1;
            const s = $$author$$project$$LBR$$serialize(msg);
            serializeDuration = mtime() - start;
            start = mtime();
            insertEvent(pos, global.config.version, s.a, updateDuration, serializeDuration, A2($$elm$$core$$Maybe$$withDefault, null, s.b));
            logDuration = mtime() - start;
          }

          // console.log(`model size: ${global.sizeof(pair.a)}`);
          // console.log(pair.a);

          stepper(model = pair.a, viewMetadata);
          //console.log('cmds', pair.b);
          _Platform_enqueueEffects(managers, pair.b, subscriptions(model));

          const stepEnqueueDuration = mtime() - start;

          if (isBackend) {
            //console.log({serialize: serializeDuration, log: logDuration, update: updateDuration, stepEnqueue: stepEnqueueDuration})
          }
        }

        if ((args && args['model']) === undefined) {
          _Platform_enqueueEffects(managers, initPair.b, subscriptions(model));
        }

        $exportFns

        const die = function() {
          //console.log('App dying');

          // Needed to stop Time.every subscriptions.
          // This must be done before clearing the stuff below.
          _Platform_enqueueEffects(managers, _Platform_batch(_List_Nil), _Platform_batch(_List_Nil));

          managers = null;
          model = null;
          stepper = null;
          ports = null;
          _Platform_effectsQueue = [];
        }

        return ports ? {
          ports: ports,
          gm: function() { return model },
          eum: function() { upgradeMode = true },
          die: die,
          fns: fns
        } : {};
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
elmPkgJs :: Mode.Mode -> B.Builder
elmPkgJs mode =
  case mode of
    Mode.Dev _ -> do
      unsafePerformIO $ do
        root <- getProjectRoot "elmPkgJs"
        elmPkgJsSources <- safeListDirectory $ root </> "elm-pkg-js"

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
                    Lamdera.Relative.loadFile $ root </> "elm-pkg-js-includes.min.js"
                  Nothing ->
                    error "Could not find path to node"

                case minFile of
                  Just minFileContents -> do
                    pure $ Ext.Common.textToBuilder minFileContents
                  Nothing -> do
                    error "no min file after compile, run `node esbuild.config.js` to check errors"
              else do
                Lamdera.debug_ "🏗️🟠  Using dumbJsPackager, ignoring esbuild.config.js in non-dev mode"
                dumbJsPackager root elmPkgJsSources
          (_, Just esbuildPath, Just includesPath) ->
            if Ext.Common.isDebug_
              then do
                esbuildIncluder root esbuildPath includesPath
              else do
                Lamdera.debug_ "🏗️🟠  Using dumbJsPackager, ignoring esbuild in non-dev mode"
                dumbJsPackager root elmPkgJsSources
          _ -> do
            Lamdera.debug_ "🏗️  Using dumbJsPackager"
            dumbJsPackager root elmPkgJsSources
    _ ->
      ""


esbuildIncluder :: FilePath -> FilePath -> FilePath -> IO B.Builder
esbuildIncluder root esbuildPath includesPath = do
  minFile <- Lamdera.Relative.loadFile $ root </> "elm-pkg-js-includes.min.js"
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




-- Tries to be clever by injecting `{}` as the `exports` value. Falls over if the target files have been compiled
-- by a packager or if they don't use the `export.init` syntax, i.e. `export async function init() {...}`
dumbJsPackager root elmPkgJsSources = do
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
