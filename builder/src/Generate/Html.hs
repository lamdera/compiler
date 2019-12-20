{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import Text.RawString.QQ (r)

import qualified Elm.Name as N

import qualified Lamdera
import qualified File.IO as IO
import qualified Data.Text.Encoding as Text
-- import Develop.StaticFiles (lamderaLive)


import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath ((</>))


-- SANDWICH


sandwich :: N.Name -> B.Builder -> B.Builder
sandwich moduleName javascript =
  let name = N.toBuilder moduleName

      lamderaLiveSrc =
        Lamdera.unsafe $ do
          debug <- Lamdera.isDebug
          if debug
            then do
              Lamdera.debug "Using elmx/ui/browser/dist/live.js for lamderaLive"
              res <- IO.readUtf8 "/Users/mario/dev/projects/elmx/ui/browser/dist/live.js"
              pure (Text.encodeUtf8Builder (Text.decodeUtf8 res))
            else
              pure (Text.encodeUtf8Builder (Text.decodeUtf8 lamderaLive))

  in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>|] <> name <> [r|</title>
</head>

<body>
<div id="elm-f0111bc4e658d0f98db96260c16f7e49"></div>
<script>
|] <> javascript <> [r|

var app = Elm.|] <> name <> [r|.init({ node: document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49") });
if (document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49"))
{
  document.getElementById("elm-f0111bc4e658d0f98db96260c16f7e49").innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.';
}
|] <> lamderaLiveSrc <> [r|
</script>
</body>
</html>|]


-- LAMDERA

lamderaLive :: BS.ByteString
lamderaLive =
  $(bsToExp =<< runIO (BS.readFile ("ui" </> "browser" </> "dist" </> "live.js")))
