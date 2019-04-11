{-# LANGUAGE OverloadedStrings #-}
module East.Hacks (injectKernelImport) where

import qualified AST.Canonical as C
import qualified Reporting.Annotation as A
import qualified Elm.Name as N

import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Language.Haskell.Exts.Simple.Pretty as HsPretty
import qualified Language.Haskell.Exts.Simple.Syntax as Hs

type List a = [a]



injectKernelImport hast =
  if List.isInfixOf "Lamdera.Haskelm.Kernel." (HsPretty.prettyPrint hast) then
    -- inject kernel import
    let
      (Hs.Module mh@(Just (Hs.ModuleHead (Hs.ModuleName moduName) _ _)) mp imports decls) = hast
    in
      Hs.Module
        mh
        mp
        (imports ++ [
            Hs.ImportDecl
            (Hs.ModuleName $ "Lamdera.Haskelm.Kernel." ++ moduName)
            True -- imported qualified?
            False -- imported with {-# SOURCE #-}?
            False -- import safe?
            Nothing -- imported with explicit package name
            Nothing -- optional alias name in an as clause.
            Nothing -- optional list of import specifications
        ])
        decls
  else
    -- not depending on a kernel module; leave it as-is
    hast
