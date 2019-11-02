Use a local package override instead of remote.

```
git clone https://github.com/Lamdera/core ~/lamdera/haskelm/pkg-overrides/packages/Lamdera/core/1.0.0

$ rm -rf elm-stuff/ elm-home/
$ LAMDERA_PKG_PATH=~/lamdera/haskelm/pkg-overrides ELM_HOME=elm-home elmx make src/Simple.elm
```

Note that core/codecs pull from static.lamdera.com, not github.

See /Users/mario/dev/projects/elmx/builder/src/Deps/Website.hs:fetchLocal
