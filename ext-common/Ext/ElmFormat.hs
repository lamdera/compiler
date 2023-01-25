{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.ElmFormat where

{- Helpers to detect and use the `elm-format` binary on the local system
-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process
import qualified System.Directory as Dir
import qualified Lamdera
import StandaloneInstances

import qualified ElmFormat.Cli
-- import qualified ElmFormat.Render.Text as Render
import ElmVersion
import ElmFormat.Messages


formatWithEmbedded :: Text -> Either ElmFormat.Messages.InfoMessage Text
formatWithEmbedded inputText = do
  ElmFormat.Cli.format ElmVersion.Elm_0_19 ("stdin:nofilepath", inputText)


format :: Text -> (Either Text Text)
format text = do
  case formatWithEmbedded text of
    Left err ->
      Left $ Lamdera.show_ err
    Right formatted ->
      Right formatted


formatOrPassthrough :: Text -> Text
formatOrPassthrough text = do
  case format text of
    Right formatted -> formatted
    Left err -> do
      -- let !_ = Lamdera.debug $ "🔥💅 warning: " <> show err
      text



-- Old versions that rely on local elm-format binary

format_ :: Text -> IO (Either Text Text)
format_ text = do
  elmFormatPath_ <- Dir.findExecutable "elm-format"
  case elmFormatPath_ of
    Just elmFormatPath -> do
      -- Lamdera.debug $ "💅 elm-formatting from stdin:\n" <> T.unpack text
      Lamdera.debug $ "💅 elm-formatting from stdin"
      (exit, stdout, stderr) <-
        System.Process.readProcessWithExitCode elmFormatPath ["--stdin"] (T.unpack text)

      if stderr /= ""
        then
          pure $ Left $ T.pack stderr
        else
          pure $ Right $ T.pack stdout

    Nothing -> do
      Lamdera.debug $ "🔥💅 warning: no elm-format found locally, skipping"
      pure $ Left $ "no elm-format found locally, skipping."


formatOrPassthrough_ :: Text -> IO Text
formatOrPassthrough_ text = do
  formatted_ <- format_ text
  case formatted_ of
    Right formatted -> pure formatted
    Left err -> do
      Lamdera.debug $ "🔥💅 warning: " <> show err
      pure text
