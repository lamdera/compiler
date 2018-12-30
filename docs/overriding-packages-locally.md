Now we can override packages locally, so we can patch other people's published packages, or do quick local development of Lamdera/core.

Local packages are installed like this:
(replace `Lamdera/core` and `1.0.0` as appropriate)
(assuming `$LAMDERA_PKG_PATH` = `lamdera-pkg-path`)
```
git clone -b 1.0.0 --single-branch --depth 1 https://github.com/Lamdera/core lamdera-pkg-path/packages/Lamdera/core/1.0.0
```

(NOTE: there's a special `$LAMDERA_PKG_PATH` that's used in builds made by the lamdera build system, and it's `~/lamdera/haskelm/pkg-overrides`. There's a readme in there with more info.)

Then we use it like this:
```
~/drathier/elm-compiler/user-code$ rm -rf elm-stuff/ elm-home/
~/drathier/elm-compiler/user-code$ LAMDERA_PKG_PATH=lamdera-pkg-path ELM_HOME=elm-home elmx make src/Simple.elm 
failed to find local file at lamdera-pkg-path/all-packages
found local file at lamdera-pkg-path/packages/Lamdera/core/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/browser/1.0.1/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/bytes/1.0.7/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/core/1.0.2/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/html/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/http/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/json/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/time/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/url/1.0.0/elm.json
failed to find local file at lamdera-pkg-path/packages/elm/virtual-dom/1.0.2/elm.json
Starting downloads...

found local pkg at lamdera-pkg-path/packages/Lamdera/core/1.0.0
Shelly.cp_r lamdera-pkg-path/packages/Lamdera/core/1.0.0 elm-home/0.19.0/package/Lamdera/core
failed to find local pkg at lamdera-pkg-path/packages/elm/browser/1.0.1
failed to find local file at lamdera-pkg-path/packages/elm/browser/1.0.1/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/bytes/1.0.7
failed to find local file at lamdera-pkg-path/packages/elm/bytes/1.0.7/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/core/1.0.2
failed to find local file at lamdera-pkg-path/packages/elm/core/1.0.2/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/html/1.0.0
failed to find local file at lamdera-pkg-path/packages/elm/html/1.0.0/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/http/1.0.0
failed to find local file at lamdera-pkg-path/packages/elm/http/1.0.0/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/json/1.0.0
failed to find local file at lamdera-pkg-path/packages/elm/json/1.0.0/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/time/1.0.0
failed to find local file at lamdera-pkg-path/packages/elm/time/1.0.0/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/url/1.0.0
failed to find local file at lamdera-pkg-path/packages/elm/url/1.0.0/endpoint.json
failed to find local pkg at lamdera-pkg-path/packages/elm/virtual-dom/1.0.2
failed to find local file at lamdera-pkg-path/packages/elm/virtual-dom/1.0.2/endpoint.json
  ● elm/bytes 1.0.7
  ● elm/http 1.0.0
  ● elm/time 1.0.0
  ● elm/virtual-dom 1.0.2
  ● elm/json 1.0.0
  ● elm/browser 1.0.1
  ● elm/html 1.0.0
  ● elm/url 1.0.0
  ● elm/core 1.0.2

...

Success! Compiled 5 modules.                                         
~/drathier/elm-compiler/user-code$ 
```

Then when you're done, delete the local copy of the package in `$LAMDERA_PKG_PATH` to use the public version. Or stop setting `LAMDERA_PKG_PATH`; if it's not there, `elmx` will behave like it did before I added this feature.
