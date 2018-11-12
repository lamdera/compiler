{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( DocsFlag(..)
  , compile
  , Artifacts(..)
  )
  where


import qualified Data.ByteString as BS
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

import System.IO.Unsafe (unsafePerformIO)

import qualified Wire.Valid
import qualified Wire.Base
import qualified East.Conversion as East

import qualified Language.Haskell.Exts.Simple.Syntax as Hs


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

      -- {- EVERGREEN
      -- Generate stubbed data calls for the functions that will be generated
      let validStubbed_ = Wire.Valid.stubValid valid flag pkg importDict interfaces source
      -- EVERGREEN -}


      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces validStubbed_

      -- {- EVERGREEN
      -- Generate and inject Evergreen functions for all types & unions
      let canonical_ = Wire.Base.modifyCanonical canonical flag pkg importDict interfaces source


      -- Backfill generated valid AST for generated functions as well
      let valid_ = Wire.Valid.modify validStubbed_ flag pkg importDict interfaces source canonical_
      -- EVERGREEN -}


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
        East.transpile canonical annotations importDict

      Result.ok $
        Artifacts
          { _elmi = I.fromModule annotations canonical_
          , _elmo = graph
          , _haskelmo = haskAst
          , _docs = documentation
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
    NoDocs ->
      Result.ok Nothing

    YesDocs ->
      Just <$> Docs.fromModule modul
