{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Live where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import System.FilePath as FP
import Control.Exception (finally, throw)
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)

import Lamdera

lamderaLiveSrc =
  Lamdera.unsafe $ do
    debug <- Lamdera.isDebug
    if debug
      then do
        let overridePath = "/Users/mario/dev/projects/lamdera-compiler/extra/dist/live.js"

        overrideM <- readUtf8Text overridePath
        case overrideM of
          Just override -> do
            Lamdera.debug "Using extra/dist/live.js for lamderaLive"
            pure (T.encodeUtf8Builder override)
          Nothing ->
            pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))
      else
        pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))


lamderaLive :: BS.ByteString
lamderaLive =
  $(bsToExp =<< runIO (BS.readFile ("extra" </> "dist" </> "live.js")))


x = 1
