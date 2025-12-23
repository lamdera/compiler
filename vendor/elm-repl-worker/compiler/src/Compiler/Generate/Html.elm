{- MANUALLY FORMATTED -}
module Compiler.Generate.Html exposing
  ( sandwich
  )


import Compiler.Data.Name as Name



-- SANDWICH


sandwich : Name.Name -> String -> String
sandwich moduleName javascript =
  let name = Name.toBuilder moduleName in
  """<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>""" ++ name ++ """</title>
  <style>body { padding: 0; margin: 0; }</style>
</head>

<body>

<pre id="elm"></pre>

<script>
try {
""" ++ String.replace "</script>" "<\\/script>" javascript ++ """

  var app = Elm.""" ++ name ++ """.init({ node: document.getElementById("elm") });
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
</html>"""
