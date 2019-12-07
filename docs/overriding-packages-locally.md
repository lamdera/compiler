Use a local package override instead of remote.

```
git clone https://github.com/lamdera/core ~/lamdera/overrides/packages/lamdera/core/1.0.0

$ rm -rf elm-stuff/ elm-home/
$ LOVR=~/lamdera/overrides ELM_HOME=elm-home elmx make src/Simple.elm
```

Note that core/codecs pull from static.lamdera.com, not github.

See /Users/mario/dev/projects/elmx/builder/src/Deps/Website.hs:fetchLocal
