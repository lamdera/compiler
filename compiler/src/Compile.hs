{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile
  ( DocsFlag(..)
  , compile
  , Artifacts(..)
  )
  where


import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BS8
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type
import qualified Data.Text as T

import System.IO.Unsafe (unsafePerformIO)

import qualified Wire.Source
import qualified East.Conversion as East

import qualified Language.Haskell.Exts.Simple.Syntax as Hs

--import qualified Debug.Trace as DT


-- COMPILE


type Result i a =
  Result.Result i [Warning.Warning] Error.Error a


type ImportDict =
  Map.Map N.Name ModuleName.Canonical


data Artifacts =
  Artifacts
    { _elmi :: I.Interface
    , _elmo :: Opt.Graph
    , _haskelmo :: Hs.Module
    , _docs :: Maybe Docs.Module
    } deriving (Show)


compile :: DocsFlag -> Pkg.Name -> ImportDict -> I.Interfaces -> BS.ByteString -> Result i Artifacts
compile flag pkg importDict interfaces source =
  {-

  pkg: Name {_author = "elm",_project = "browser"}
       The local author+project names of the package this file belongs to

  importDict: fromList [(Name {_name = "AllTypes"}
                ,Canonical {_package = Name {_author = "author",_project = "project"}
                           ,_module = Name {_name = "AllTypes"}})
                ,(Name {_name = "Basics"}
                ,Canonical {_package = Name {_author = "elm",_project = "core"}
                           ,_module = Name {_name = "Basics"}})
                , ...
                ]
       The set of packages imported by this file

  interfaces: _types, _unions and _aliases for all imported modules


  The problem here is that AST.Valid doesn't contain canonicalised information.

  As a result, we can't accurately generate Evergreen for non-standard types
  included from external modules as we don't yet have the canonical paths to
  know where those types come from.

  However, we can't just modify the AST.Canonical version either - the original
  AST.Valid value is still used elsewhere and if they disagree it causes probelms.

  So it seems we'll need to:

  1. Inject dummy declarations into `valid`
  2. Inject proper implementations into `canonical`
  3. Backfill proper implementations into `valid`

  Because type-inference doesn't come till a later stage, we should be ok with this funny business.

  -}
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program pkg source

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid

      -- generate wire source code from canonical ast
      -- these are intended to be serialised and put at the end of source, then we redo the whole compilation step, generating valid as normal etc.
      rawCodecSource <- pure $ T.unpack $ Wire.Source.generateCodecs canonical

      let newSource =
            if Map.lookup "Lamdera.Evergreen" importDict == Nothing then -- Evergreen isn't in the importDict, so this is a kernel module, or something that shouldn't have access to Evergreen, like the Evergreen module itself.
              source
            else
              BS8.fromString (Wire.Source.injectEvergreenExposing canonical (Wire.Source.injectEvergreenImport (BS8.toString source))) <> "\n\n-- ### codecs\n" <> BS8.fromString rawCodecSource

      valid_ <- Result.mapError Error.Syntax $
        --DT.trace (BS8.toString newSource) $ -- uncomment to print source code for all modules
        Parse.program pkg newSource

      canonical_ <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid_

      let localizer = L.fromModule valid_ -- TODO should this be strict for GC?

      annotations <-
        runTypeInference localizer canonical_

      () <-
        exhaustivenessCheck canonical_

      graph <- Result.mapError (Error.Main localizer) $
        Optimize.optimize annotations canonical_

      documentation <-
        genarateDocs flag canonical_

      haskAst <-
        East.transpile canonical_ annotations importDict


      Result.ok $
        Artifacts
          { _elmi = I.fromModule annotations canonical_
          --{ _elmi = Wire.Canonical.reinjectWireInterfaces annotations canonical_ $ I.fromModule annotations canonical_
          , _elmo = graph
          , _haskelmo = haskAst
          , Compile._docs = documentation
          }


-- TYPE INFERENCE


runTypeInference :: L.Localizer -> Can.Module -> Result i (Map.Map N.Name Can.Annotation)
runTypeInference localizer canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      Result.ok annotations

    Left errors ->
      Result.throw (Error.Type localizer errors)



-- EXHAUSTIVENESS CHECK


exhaustivenessCheck :: Can.Module -> Result i ()
exhaustivenessCheck canonical =
  case PatternMatches.check canonical of
    Left errors ->
      Result.throw (Error.Pattern errors)

    Right () ->
      Result.ok ()



-- DOCUMENTATION


data DocsFlag = YesDocs | NoDocs deriving (Show)


genarateDocs :: DocsFlag -> Can.Module -> Result.Result i w Error.Error (Maybe Docs.Module)
genarateDocs flag modul =
  case flag of
    Compile.NoDocs ->
      Result.ok Nothing

    Compile.YesDocs ->
      Just <$> Docs.fromModule modul
