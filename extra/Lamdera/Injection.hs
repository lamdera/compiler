{-# LANGUAGE OverloadedStrings #-}

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

import qualified Generate.Mode as Mode

import Lamdera


source :: B.Builder
source =
  unsafePerformIO $ do

    injectionsM <- Env.lookupEnv "BACKENDINJECTION"

    Lamdera.debug_ $ "Got " <> show injectionsM <> " for BACKENDINJECTION"

    case injectionsM of
      Just injectionsPath -> do
        Lamdera.debug_ $ "Injecting " <> injectionsPath <> " into final source"
        B.byteString <$> File.readUtf8 injectionsPath

      Nothing ->
        -- No injections, so we'll inject empty string
        pure ""


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
