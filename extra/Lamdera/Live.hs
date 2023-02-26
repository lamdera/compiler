{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Live where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import System.FilePath as FP
import Control.Exception (finally, throw)
import Language.Haskell.TH (runIO)
import Data.FileEmbed (bsToExp)
import qualified System.Directory as Dir

import Lamdera
import qualified Lamdera.RelativeLoad
import qualified Ext.Common

lamderaLiveSrc =
  Lamdera.unsafe $
    if Ext.Common.isDebug_
      then do
        Lamdera.debug $ "🗿  Using debug mode lamderaLive"
        userHome <- Dir.getHomeDirectory
        let overrideRoot = userHome </> "dev/projects/lamdera-compiler/extra"
            overridePath = overrideRoot </> "live.js"
            overridePathBuilt = overrideRoot </> "dist/live.js"

        exists <- doesFileExist overridePath
        if exists
          then do
            Lamdera.debug $ "🗿 Using " ++ overridePathBuilt ++ " for lamderaLive"
            Ext.Common.requireBinary "npm"
            Ext.Common.requireBinary "esbuild"
            Ext.Common.bash $ "cd " <> overrideRoot <> " && npm ci && esbuild " <> overridePath <> " --bundle --minify --target=chrome58,firefox57,safari11,edge16 > " <> overridePathBuilt
            overrideM <- readUtf8Text overridePathBuilt
            case overrideM of
              Just override -> do
                pure (T.encodeUtf8Builder override)
              Nothing -> do
                Lamdera.debug $ "Couldn't load override " ++ overridePath ++ ", using compiled lamderaLive"
                pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))
          else do
            Lamdera.debug $ "Couldn't find override " ++ overridePath ++ ", using compiled lamderaLive"
            pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))
      else do
        Lamdera.debug $ "🗿  Using compiled lamderaLive"
        pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))


-- @TODO means we have to restart live for any changes... how to improve that?
lamderaLiveHead :: FilePath -> B.Builder
lamderaLiveHead root =
  Lamdera.unsafe $ do
    headHtmlM <- readUtf8Text $ root </> "head.html"
    case headHtmlM of
      Just headHtml ->
        pure (T.encodeUtf8Builder headHtml)

      Nothing ->
        pure ""


lamderaLive :: BS.ByteString
lamderaLive =
  $(bsToExp =<< runIO (Lamdera.RelativeLoad.find "extra/dist/live.js"))
