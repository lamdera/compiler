{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Injection where

import System.IO.Unsafe (unsafePerformIO)
import qualified File.IO as IO
import qualified System.Environment as Env
import qualified Data.ByteString.Builder as B

import Lamdera


source :: B.Builder
source =
  unsafePerformIO $ do

    injectionsM <- Env.lookupEnv "BACKENDINJECTION"

    Lamdera.debug_ $ "Got " <> show injectionsM <> " for BACKENDINJECTION"

    case injectionsM of
      Just injectionsPath -> do
        Lamdera.debug_ $ "Injecting " <> injectionsPath <> " into final source"
        B.byteString <$> IO.readUtf8 injectionsPath

      Nothing ->
        -- No injections, so we'll inject empty string
        pure ""
