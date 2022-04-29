module Shakefiles.Dependencies (rules) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


localBinDir :: String
localBinDir = "bin"

--ghc-option required because of https://gitlab.haskell.org/ghc/ghc/-/issues/20592
ghcOptionFFI = "--ghc-option=-I/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/ffi"


cabalInstallExe :: String -> Action ()
cabalInstallExe package =
    cmd_ "cabal"
        [ "v2-install"
        , package
        , "--installdir", localBinDir
        -- these are currently needed because Windows doesn't support the default symlink method
        , "--install-method=copy"
        , "--overwrite-policy=always"
        , ghcOptionFFI
        ]


rules :: Rules FilePath
rules = do
    let shellcheck = localBinDir </> "shellcheck" <.> exe

    phony "dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        , "_build/cabal-test-dependencies.ok"
        , shellcheck
        ]
    phony "dist-dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        ]

    "_build/cabal-dependencies.ok" %> \out -> do
        need
            [ "elm.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" [ "v2-build", "--only-dependencies", ghcOptionFFI ]
        writeFile' out ""

    "_build/cabal-test-dependencies.ok" %> \out -> do
        need
            [ "elm.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" [ "v2-build", "--only-dependencies", "--enable-tests", ghcOptionFFI ]
        writeFile' out ""

    shellcheck %> \out -> do
        cabalInstallExe "ShellCheck"

    return shellcheck
