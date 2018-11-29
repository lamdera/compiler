{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire.Interfaces where

import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N

import AST.Canonical
import qualified AST.Module.Name as ModuleName
import Elm.Package (Name(..))
import Elm.Interface as Interface
import qualified Data.Map as Map
import Data.Index
import qualified Data.List as List

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint
import Wire.Helpers


trace a b =
  unsafePerformIO $ do
    print $ show a ++ ": " ++ show b
    pure b


modifyInterfaces ifaces =
  Map.mapWithKey modifyInterface ifaces


modifyInterface moduleName interface =
  let
    aliasTypeStubs :: [(N.Name, Annotation)]
    aliasTypeStubs = foldl (++) [] $ fmap (generateTypeStubAliases moduleName) (Map.toList $ Interface._aliases interface)

    unionTypeStubs :: [(N.Name, Annotation)]
    unionTypeStubs = foldl (++) [] $ fmap (generateTypeStubUnions moduleName) (Map.toList $ Interface._unions interface)

    typeStubs = (aliasTypeStubs) ++ (unionTypeStubs)

    types = Map.toList $ Interface._types interface
  in
    interface { _types = Map.fromList $ types ++ (trace ("typeStubs:" ++ show moduleName) typeStubs) }



data Alias = Alias [N.Name] Type deriving (Show)

generateTypeStubAliases :: ModuleName.Canonical -> (N.Name, Interface.Alias) -> [(N.Name, Annotation)]
generateTypeStubAliases moduleCanonical (name, alias_) =
  let
    encoderName = "evg_e_" ++ N.toString name

    encoderType =
      Forall (Map.fromList [])
        (tlam (qtypc moduleCanonical (N.toString name) [])
          (qtyp "elm" "json" "Json.Encode" "Value" []))

    generations =
      case alias_ of
        PublicAlias alias ->
          [(N.fromString $ encoderName, encoderType)
          ,(N.fromString $ "evg_d_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ]

        PrivateAlias alias ->
          [(N.fromString $ encoderName, encoderType)
          ,(N.fromString $ "evg_d_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ]

  in
  case moduleCanonical of
    ModuleName.Canonical (Name "author" "project") moduleNameLocal ->
      generations

    _ -> [] -- @TODO skip extensive stuff for now...



data Union =
  Union
    { _u_vars :: [N.Name]
    , _u_alts :: [Ctor]
    , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
    , _u_opts :: CtorOpts -- CACHE which optimizations are available
    }
    deriving (Show)

generateTypeStubUnions :: ModuleName.Canonical -> (N.Name, Interface.Union) -> [(N.Name, Annotation)]
generateTypeStubUnions moduleCanonical (name, union_) =
  let
    generations =
      case union_ of
        OpenUnion union ->
          [(N.fromString $ "evg_d_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ,(N.fromString $ "evg_e_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ]


        ClosedUnion union ->
          [(N.fromString $ "evg_d_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ,(N.fromString $ "evg_e_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ]


        PrivateUnion union ->
          [(N.fromString $ "evg_d_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ,(N.fromString $ "evg_e_" ++ N.toString name, (Forall (Map.fromList []) (TUnit)))
          ]

  in
  case moduleCanonical of
    ModuleName.Canonical (Name "author" "project") moduleNameLocal ->
      generations

    _ -> [] -- @TODO skip extensive stuff for now...





  --
  --
  --
  -- , _unions  :: Map.Map N.Name Union
  -- , _aliases :: Map.Map N.Name Alias
  --
  --
  --
  -- data Union
  --   = OpenUnion Can.Union
  --   | ClosedUnion Can.Union
  --   | PrivateUnion Can.Union
  --   deriving (Show)
  --
  --
  -- data Alias
  --   = PublicAlias Can.Alias
  --   | PrivateAlias Can.Alias
  --   deriving (Show)
