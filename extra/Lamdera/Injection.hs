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
import NeatInterpolation

import qualified Generate.Mode as Mode

import Lamdera


source :: B.Builder
source =
  {- Approach taken from cors-anywhere:
  https://github.com/Rob--W/cors-anywhere/blame/master/README.md#L56

  Rewrites all XHR requests from {url} to http://localhost:9000/{url}

  See extra/Lamdera/ReverseProxy.hs for the proxy itself
  -}
  [text|
    (function() {
      var cors_api_host = 'localhost:9000';
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
  & Text.encodeUtf8
  & B.byteString


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
          , "window.elmPkgJsInit = function(app) {"
          , "  for (var pkgId in pkgExports) {"
          , "    if (pkgExports.hasOwnProperty(pkgId)) {"
          , "      pkgExports[pkgId]({}).init(app)"
          , "    }"
          , "  }"
          , "}"
          ]

    _ ->
      ""
