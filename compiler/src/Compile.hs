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
import qualified AST.Source as Src
import qualified AST.Valid as Valid
import qualified Reporting.Region as R
import qualified Reporting.Annotation as A
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
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)

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


compile :: DocsFlag -> Pkg.Name -> ImportDict -> I.Interfaces -> BS.ByteString -> MVar BS.ByteString -> Result i Artifacts
compile flag pkg importDict interfaces source srcMVar =
  -- This is the main function for compiling a single Elm module.
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program pkg source

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid

      let localizer = L.fromModule valid -- TODO should this be strict for GC?

      annotations <-
        runTypeInference localizer canonical

      () <-
        exhaustivenessCheck canonical

      graph <- Result.mapError (Error.Main localizer) $
        Optimize.optimize annotations canonical

      documentation <-
        genarateDocs flag canonical

      --
      -- Ok, normal elm compilation chain is now done for this module, so any normal elm errors which may have happened will have been found and returned by now. This should reduce confusion for devs. Next, after the elm code is known good, we generate evergreen codecs, inject them, and then run the whole compilation step once more, with generated code this time.
      -- This also gives us a free pass from our previous discussion on how many compilation steps we should run before we inject codecs; more steps is slower but more correct. Now we need to run all steps, so we're free to use all information if we want to.
      --

      -- generate wire source code from canonical ast
      -- these are intended to be serialised and put at the end of the source code string, then we redo the whole module compilation with this new source injected.
      rawCodecSource <- pure $ T.unpack $ Wire.Source.generateCodecs (getImportDict valid) canonical

      valid_ <- Result.mapError Error.Syntax $
        if Map.lookup "Lamdera.Evergreen" importDict == Nothing then -- Evergreen isn't in the importDict, so this is a kernel module, or something that shouldn't have access to Evergreen, like the Evergreen module itself.
          Parse.program pkg source
        else
          let newSource =
                BS8.fromString (Wire.Source.injectEvergreenExposing canonical (BS8.toString source)) <> "\n\n-- ### codecs\n" <> BS8.fromString rawCodecSource
              !_ = unsafePerformIO $ do
                    -- yolo; put the new source into an mvar, so we can communicate upstream to the scheduler that we've modified the input, so it can use the modified source form here on when generating error messages. I tried, but I didn't see a better option to accomplish this than a mutable variable.
                    _ <- takeMVar srcMVar -- drop the old source code
                    putMVar srcMVar newSource -- insert the new source code
          in
            --DT.trace (BS8.toString newSource) $ -- uncomment to print source code for all modules
            -- it's safer to add stuff to the parsed result, but much harder to debug, so codecs are generated as source code, and imports are added like this now
            addImport (Src.Import (A.At R.lamderaInject "Lamdera.Evergreen") Nothing (Src.Explicit [])) <$>
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
          , _elmo = graph
          , _haskelmo = haskAst
          , Compile._docs = documentation
          }

addImport :: Src.Import -> Valid.Module -> Valid.Module
addImport i (Valid.Module _name _overview _docs _exports _imports _decls _unions _aliases _binop _effects) =
  Valid.Module _name _overview _docs _exports (i : _imports) _decls _unions _aliases _binop _effects

getImportDict :: Valid.Module -> Map.Map N.Name N.Name
getImportDict (Valid.Module _name _overview _docs _exports _imports _decls _unions _aliases _binop _effects) =
  Map.fromList $
  (\(Src.Import (A.At _ _import) _alias _exposing) ->
    case _alias of
      Just al -> (_import, al)
      Nothing -> (_import, _import)
  ) <$> _imports


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
