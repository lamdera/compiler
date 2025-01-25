{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.ElmFormat where

{- Helpers to detect and use the `elm-format` binary on the local system
-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import System.IO (FilePath)

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process
import qualified System.Directory as Dir
import qualified Lamdera
import StandaloneInstances

import qualified ElmFormat.Cli
-- import qualified ElmFormat.Render.Text as Render
import ElmVersion
import ElmFormat.Messages
import Reporting.Annotation (Located(..), Region(..), Position(..))
import CommandLine.InfoFormatter (ToConsole(..))


formatWithEmbedded :: FilePath -> Text -> Either ElmFormat.Messages.InfoMessage Text
formatWithEmbedded filePath inputText = do
  ElmFormat.Cli.format ElmVersion.Elm_0_19 (filePath, inputText)


format :: FilePath -> Text -> (Either Text Text)
format filePath text = do
  case formatWithEmbedded filePath text of
    Left err -> Left $ toConsole err
    Right formatted -> Right formatted


formatOrPassthrough :: Text -> Text
formatOrPassthrough text = do
  case format "stdin" text of
    Right formatted -> formatted
    Left _ -> text


formatOrPassthroughFile :: FilePath -> Text -> Text
formatOrPassthroughFile filePath text = do
  case format filePath text of
    Right formatted -> formatted
    Left _ -> text


-- Old versions that rely on local elm-format binary

-- format_ :: Text -> IO (Either Text Text)
-- format_ text = do
--   elmFormatPath_ <- Dir.findExecutable "elm-format"
--   case elmFormatPath_ of
--     Just elmFormatPath -> do
--       -- Lamdera.debug $ "💅 elm-formatting from stdin:\n" <> T.unpack text
--       Lamdera.debug $ "💅 elm-formatting from stdin"
--       (exit, stdout, stderr) <-
--         System.Process.readProcessWithExitCode elmFormatPath ["--stdin"] (T.unpack text)

--       if stderr /= ""
--         then
--           pure $ Left $ T.pack stderr
--         else
--           pure $ Right $ T.pack stdout

--     Nothing -> do
--       Lamdera.debug $ "🔥💅 warning: no elm-format found locally, skipping"
--       pure $ Left $ "no elm-format found locally, skipping."


-- formatOrPassthrough_ :: Text -> IO Text
-- formatOrPassthrough_ text = do
--   formatted_ <- format_ text
--   case formatted_ of
--     Right formatted -> pure formatted
--     Left err -> do
--       Lamdera.debug $ "🔥💅 warning: " <> show err
--       pure text
