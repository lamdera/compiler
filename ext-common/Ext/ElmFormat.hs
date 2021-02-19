{-# LANGUAGE OverloadedStrings #-}

module Ext.ElmFormat where

{- Helpers to detect and use the `elm-format` binary on the local system
-}

import Data.Text (Text)
import qualified Data.Text as T

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process
import qualified System.Directory as Dir


format :: Text -> IO (Either Text Text)
format text = do
  elmFormatPath_ <- Dir.findExecutable "elm-format"
  case elmFormatPath_ of
    Just elmFormatPath -> do
      (exit, stdout, stderr) <-
        System.Process.readProcessWithExitCode elmFormatPath ["--stdin"] (T.unpack text)

      if stderr /= ""
        then
          pure $ Left $ T.pack stderr
        else
          pure $ Right $ T.pack stdout

    Nothing -> do
      pure $ Left $ "No elm-format found locally, skipping."


formatOrPassthrough :: Text -> IO Text
formatOrPassthrough text = do
  formatted_ <- format text
  case formatted_ of
    Right formatted -> pure formatted
    Left err -> pure text
