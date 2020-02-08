// This attempt didn't work out too well, as the exe's explode in competition
// with each other for some unknown reason. Leaving this here for future reference,
// but for now just manually managing inclusion/exclusion of tests

name: elm-test
version: '0.19.0'
synopsis: The `elm` command line interface.
description: ! 'This includes commands like `elm make`, `elm repl`, and many others

  for helping make Elm developers happy and productive.'
category: Compiler, Language
author: Evan Czaplicki
maintainer: info@elm-lang.org
copyright: Copyright (c) 2011-present, Evan Czaplicki
license: BSD3
homepage: http://elm-lang.org
git: git://github.com/elm-lang/elm-compiler.git
other-extensions:
- TemplateHaskell
dependencies:
- ansi-terminal >=0.8 && <0.9
- ansi-wl-pprint >=0.6.8 && <0.7
- base >=4.8 && <5
- binary >=0.8 && <0.9
- bytestring >=0.9 && <0.11
- containers >=0.6.0.1 && <0.7
- directory >=1.2.3.0 && <2.0
- edit-distance >=0.2 && <0.3
- file-embed
- filepath >=1 && <2.0
- ghc-prim
- haskeline
- HTTP >=4000.2.5 && <4000.4
- http-client >=0.5 && <0.6
- http-client-tls >=0.3 && <0.4
- connection >=0.2.8
- http-types >=0.9 && <1.0
- language-glsl
- lens
- logict
- mtl >=2.2.1 && <3
- network >=2.4 && <2.9
- parsec
- process
- raw-strings-qq
- scientific
- SHA
- snap-core
- snap-server
- template-haskell
- text >=1 && <2
- time
- unordered-containers
- uniplate
- utf8-string
- vector
- zip-archive <0.4
- show-prettyprint
- ilist
- witherable
- neat-interpolation
- natural-sort
- listsafe
- websockets
- websockets-snap
- stm
- fsnotify
- unicode-show
executables:
  elmx:
    main: Main.hs
    source-dirs:
    - compiler/src
    - builder/src
    - ui/terminal/src
    - extra
    when:
    - condition: flag(dev)
      then:
        ghc-options:
        - -O0
        - -Wall
        - -Werror
        - -Wno-error=name-shadowing
        - -Wno-error=type-defaults
        - -Wno-missing-signatures
        - -Wmissing-exported-signatures
        - -Wno-error=unused-top-binds
      else:
        ghc-options:
        - -O2
        - -fsimpl-tick-factor=200
        - -rtsopts

  elmx-test:
    main: Tests.hs
    source-dirs:
    - compiler/src
    - builder/src
    - ui/terminal/src
    - extra
    - test
    dependencies:
    - async
    - random
    - tree-diff

flags:
  dev:
    description: Turn off optimization and make warnings errors
    manual: false
    default: false
