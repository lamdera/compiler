{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lamdera.CLI.Live.Output where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Name as Name
import Text.RawString.QQ (r)

import qualified Lamdera
import qualified Lamdera.Live
import qualified Lamdera.UiSourceMap


-- For when we already have the combined JS from cache and just need to wrap the HTML around it
parts :: FilePath -> Name.Name -> B.Builder -> (B.Builder, B.Builder, B.Builder)
parts root moduleName javascript =
  let
    name = Name.toBuilder moduleName

    (hasCustom, customHead) = Lamdera.unsafe $ Lamdera.Live.lamderaLiveHead root
    htmlHead =
      if hasCustom
        then
          customHead
        else
          "<title>" <> name <> "</title>"

    combinedjs = js root moduleName javascript

    pre = [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=5.0, minimum-scale=1.0">
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <style>body { padding: 0; margin: 0; } body.boot-unhandled-js-error { padding: 10px } @media (prefers-color-scheme: dark) { body.boot-unhandled-js-error { background-color: #000; color: #fff; } } </style>
  |] <> htmlHead <> [r|
</head>

<body>

<pre id="elm"></pre>

<script>
|]

    post = [r|
</script>

</body>
</html>|]

  in
  (pre, combinedjs, post)


-- The Elm app, `lamdera live` JS harness (live.ts) and UI sourcemaps combined into a single JS snippet
js :: FilePath -> Name.Name -> B.Builder -> B.Builder
js root moduleName javascript =
  let
    name = Name.toBuilder moduleName
  in
  [r|
try {
|] <> javascript <> [r|
|] <> Lamdera.Live.lamderaLiveSrc <> Lamdera.UiSourceMap.src <> [r|
  setupApp("|] <> name <> [r|", "elm")
}
catch (e)
{
  // document.body.classList.add("boot-unhandled-js-error");
  // // display initialization errors (e.g. bad flags, infinite recursion)
  // var header = document.createElement("h1");
  // header.style.fontFamily = "monospace";
  // header.innerText = "Initialization Error";
  // var pre = document.createElement("pre");
  // document.body.insertBefore(header, pre);
  // pre.innerText = e;
  throw e;
}
|]


-- For when we need to generate the HTML and combined JS from scratch
-- html :: FilePath -> Name.Name -> B.Builder -> B.Builder
-- html root moduleName javascript =
--   wrapjs root moduleName (js root moduleName javascript)
