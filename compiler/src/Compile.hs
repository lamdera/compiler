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

import qualified WireAst
import qualified Wire
-- import qualified AST.Valid as AS (Module(..))

-- COMPILE


type Result i a =
  Result.Result i [Warning.Warning] Error.Error a


type ImportDict =
  Map.Map N.Name ModuleName.Canonical


data Artifacts =
  Artifacts
    { _elmi :: I.Interface
    , _elmo :: Opt.Graph
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

  1. Inject dummy declarations into `value`
  2. Inject proper implementations into `canonical`
  3. Backfill proper implementations into `value`

  Because type-inference doesn't come till a later stage, we should be ok with this funny business.

  -}
  do
      valid <- Result.mapError Error.Syntax $
        Parse.program pkg source

      -- _ <- unsafePerformIO $ do
      --   case valid of
      --     AS.Module n _ _ _ _ _ _ _ _ _ -> do
      --       putStrLn $ "For " ++ N.toString n
      --       pure (Result.ok "blah")

      let valid_ = WireAst.modify valid flag pkg importDict interfaces source

      -- _ <- unsafePerformIO $ do
      --   putStrLn "Did validation"
      --   pure (Result.ok "blah")

      canonical <- Result.mapError Error.Canonicalize $
        Canonicalize.canonicalize pkg importDict interfaces valid_
      -- canonical <- Result.mapError Error.Canonicalize $
      --   case valid of
      --     AS.Module n _ _ _ _ _ _ _ _ _ -> do
      --       Canonicalize.canonicalize
      --         (Wire.tracef ("pkg" ++ N.toString n) pkg)
      --         (Wire.tracef ("importDict" ++ N.toString n) importDict)
      --         (Wire.tracef ("interfaces" ++ N.toString n) interfaces)
      --         valid_

      let canonical_ = Wire.modify canonical flag pkg importDict interfaces source

      -- _ <- unsafePerformIO $ do
      --   putStrLn "Did canonicalization"
      --   pure (Result.ok "blah")


      let localizer = L.fromModule valid -- TODO should this be strict for GC?

      -- _ <- unsafePerformIO $ do
      --   putStrLn "Did localizer"
      --   pure (Result.ok "blah")


      annotations <-
        runTypeInference localizer canonical_

      -- _ <- unsafePerformIO $ do
      --   putStrLn "Did annotations/type inference"
      --   pure (Result.ok "blah")


      () <-
        exhaustivenessCheck canonical

      -- _ <- unsafePerformIO $ do
      --   putStrLn "Did exhaustivenessCheck"
      --   pure (Result.ok "blah")


      graph <- Result.mapError (Error.Main localizer) $
        Optimize.optimize annotations canonical

      documentation <-
        genarateDocs flag canonical

      Result.ok $
        Artifacts
          { _elmi = I.fromModule annotations canonical
          , _elmo = graph
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
