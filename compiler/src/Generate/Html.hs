{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name

import Literals (b)

import qualified Lamdera
import qualified Lamdera.Live
import qualified Lamdera.UiSourceMap

-- SANDWICH

-- @LAMDERA root :: FilePath parameter added.
sandwich :: FilePath -> Name.Name -> B.Builder -> B.Builder
sandwich root moduleName javascript =
  Lamdera.alternativeImplementationWhen Lamdera.isLamdera_ (sandwich_ root moduleName javascript) $
  let name = Name.toBuilder moduleName in
  [b|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>|] <> name <> [b|</title>
  <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre data-elm id="elm"></pre>

<script>
try {
|] <> javascript <> [b|

  var app = Elm.|] <> name <> [b|.init({ node: document.getElementById("elm") });
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


-- @LAMDERA

sandwich_ :: FilePath -> Name.Name -> B.Builder -> B.Builder
sandwich_ root moduleName javascript =
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
  [b|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=5.0, minimum-scale=1.0">
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <style>body { padding: 0; margin: 0; } body.boot-unhandled-js-error { padding: 10px } @media (prefers-color-scheme: dark) { body.boot-unhandled-js-error { background-color: #000; color: #fff; } } </style>
  |] <> htmlHead <> [b|
</head>

<body>

<pre data-elm id="elm"></pre>

<script>
try {
// lamdera-elm-js-start
|] <> javascript <> [b|
// lamdera-elm-js-end
|] <> Lamdera.Live.lamderaLiveSrc <> Lamdera.UiSourceMap.src <> [b|
  setupApp("|] <> name <> [b|", "elm")
}
catch (e)
{
  document.body.classList.add("boot-unhandled-js-error");
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
