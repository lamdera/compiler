{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import Text.RawString.QQ (r)

import qualified Lamdera
import qualified Lamdera.Live
import qualified Lamdera.UiSourceMap

-- SANDWICH


sandwich :: FilePath -> Name.Name -> B.Builder -> B.Builder
sandwich root moduleName javascript =
  let
    name = Name.toBuilder moduleName

    (hasCustom, customHead) = Lamdera.unsafe $ Lamdera.Live.lamderaLiveHead root
    htmlHead =
      if hasCustom
        then
          customHead
        else
          "<title>" <> name <> "</title>"
  in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=5.0, minimum-scale=1.0">
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <style>body { padding: 0; margin: 0; }</style>
  |] <> htmlHead <> [r|
</head>

<body>

<pre id="elm"></pre>

<script>
try {
|] <> javascript <> [r|
|] <> Lamdera.Live.lamderaLiveSrc <> Lamdera.UiSourceMap.src <> [r|
  setupApp("|] <> name <> [r|", "elm")
}
catch (e)
{
  // display initialization errors (e.g. bad flags, infinite recursion)
  var header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  var pre = document.getElementById("elm");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
</script>

</body>
</html>|]
