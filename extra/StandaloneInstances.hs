{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StandaloneInstances where

import Data.ByteString.Builder as B
import Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String (IsString, fromString)
import qualified Data.Map as Map
import qualified GHC.IORef

-- Elm modules

import qualified AST.Canonical
import qualified AST.Optimized
import qualified AST.Source
import qualified AST.Utils.Binop
import qualified AST.Utils.Shader
import qualified Data.Index
import qualified Data.Name
import qualified Data.NonEmptyList
import qualified Data.Utf8 as Utf8
import qualified Elm.Constraint
import qualified Elm.Float
import qualified Elm.Interface
import qualified Elm.Kernel
import qualified Elm.Licenses
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline
import qualified Elm.Package
import qualified Elm.String
import qualified Elm.Version
import qualified Http
import qualified Json.Decode
import qualified Json.String
import qualified Parse.Primitives
import qualified Reporting.Annotation
import qualified Reporting.Exit
import qualified Type.Type
import qualified Type.UnionFind
import qualified Reporting.Error.Type
import qualified Deps.Registry

-- Show

deriving instance Show AST.Optimized.Global
deriving instance Show AST.Optimized.GlobalGraph
deriving instance Show AST.Optimized.Node
deriving instance Show AST.Optimized.Choice
deriving instance Show AST.Optimized.Expr
instance (Show a) => Show (AST.Optimized.Decider a) where
  show _ = "\"<AST.Optimized.Decider a>\""
deriving instance Show AST.Optimized.Def
deriving instance Show AST.Optimized.Destructor
deriving instance Show AST.Optimized.Path
deriving instance Show AST.Optimized.EffectsType

deriving instance Show AST.Canonical.Type
deriving instance Show AST.Canonical.FieldType
deriving instance Show AST.Canonical.AliasType
-- deriving instance Show AST.Canonical.Decls
deriving instance Show AST.Canonical.Def
deriving instance Show AST.Canonical.Pattern_
deriving instance Show AST.Canonical.Union
deriving instance Show AST.Canonical.Ctor
deriving instance Show AST.Canonical.CtorOpts
deriving instance Show AST.Canonical.PatternCtorArg
deriving instance Show AST.Canonical.Expr_
deriving instance Show AST.Canonical.CaseBranch
deriving instance Show AST.Canonical.FieldUpdate
instance Show AST.Canonical.Annotation where
  -- data Annotation = Forall FreeVars Type
  show (AST.Canonical.Forall freevars tipe) =
    "(Forall (" ++ showMapQualified freevars ++ ") (" ++ show tipe ++ "))"

deriving instance Show AST.Canonical.Module
deriving instance Show AST.Canonical.Exports
deriving instance Show AST.Canonical.Export
deriving instance Show AST.Canonical.Decls
deriving instance Show AST.Canonical.Alias
deriving instance Show AST.Canonical.Binop
deriving instance Show AST.Canonical.Effects
deriving instance Show AST.Canonical.Port
deriving instance Show AST.Canonical.Manager

-- deriving instance Eq AST.Canonical.Type
-- deriving instance Eq AST.Canonical.FieldType
-- deriving instance Eq AST.Canonical.AliasType
-- deriving instance Eq AST.Canonical.Decls
deriving instance Eq AST.Canonical.Def
deriving instance Eq AST.Canonical.Pattern_
-- deriving instance Eq AST.Canonical.Union
-- deriving instance Eq AST.Canonical.Ctor
-- deriving instance Eq AST.Canonical.CtorOpts
deriving instance Eq AST.Canonical.PatternCtorArg
deriving instance Eq AST.Canonical.Expr_
deriving instance Eq AST.Canonical.CaseBranch
deriving instance Eq AST.Canonical.FieldUpdate
-- deriving instance Eq AST.Canonical.Annotation



deriving instance Show AST.Source.Module
deriving instance Show AST.Source.Exposing
deriving instance Show AST.Source.Exposed
deriving instance Show AST.Source.Privacy
deriving instance Show AST.Source.Docs
deriving instance Show AST.Source.Comment
deriving instance Show AST.Source.Import
deriving instance Show AST.Source.Value
deriving instance Show AST.Source.Pattern_
deriving instance Show AST.Source.Expr_
deriving instance Show AST.Source.Type_
deriving instance Show AST.Source.Union
deriving instance Show AST.Source.Alias
deriving instance Show AST.Source.Infix
deriving instance Show AST.Source.Effects
deriving instance Show AST.Source.Port
deriving instance Show AST.Source.Manager
deriving instance Show AST.Source.VarType
deriving instance Show AST.Source.Def
deriving instance Show AST.Utils.Binop.Associativity
deriving instance Show AST.Utils.Binop.Precedence
instance Show AST.Utils.Shader.Source where
  show _ = "<shader source>"
deriving instance Show AST.Utils.Shader.Types
deriving instance Show AST.Utils.Shader.Type
deriving instance Show Parse.Primitives.Snippet

-- deriving instance Eq AST.Source.Module
-- deriving instance Eq AST.Source.Exposing
-- deriving instance Eq AST.Source.Exposed
-- deriving instance Eq AST.Source.Privacy
-- deriving instance Eq AST.Source.Docs
-- deriving instance Eq AST.Source.Comment
-- deriving instance Eq AST.Source.Import
-- deriving instance Eq AST.Source.Value
-- deriving instance Eq AST.Source.Pattern_
-- deriving instance Eq AST.Source.Expr_
-- deriving instance Eq AST.Source.Type_
-- deriving instance Eq AST.Source.Union
-- deriving instance Eq AST.Source.Alias
-- deriving instance Eq AST.Source.Infix
-- deriving instance Eq AST.Source.Effects
-- deriving instance Eq AST.Source.Port
-- deriving instance Eq AST.Source.Manager
-- deriving instance Eq AST.Source.VarType
-- deriving instance Eq AST.Source.Def
--
instance Eq AST.Utils.Shader.Source where
  (==) _ _ = False
deriving instance Eq AST.Utils.Shader.Types
deriving instance Eq AST.Utils.Shader.Type
-- deriving instance Eq Parse.Primitives.Snippet




instance Show Elm.Float.Float where
  show = T.unpack . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString . Elm.Float.toBuilder

deriving instance (Show a) => Show (Json.Decode.Error a)
deriving instance (Show a) => Show (Json.Decode.Problem a)
deriving instance Show (Json.Decode.ParseError)
deriving instance Show (Json.Decode.StringProblem)
deriving instance Show (Json.Decode.DecodeExpectation)
deriving instance Show Reporting.Annotation.Region
deriving instance Show Reporting.Annotation.Position

-- deriving instance (Show a) => Show (Reporting.Annotation.Located a)
instance (Show a) => Show (Reporting.Annotation.Located a) where
  show (Reporting.Annotation.At region a) = "(a (" ++ show a ++ "))"

instance (Eq a) => Eq (Reporting.Annotation.Located a) where
  -- Comparison that ignores the actual regions – this is helpful to us in our generation checking
  -- as locations are not important for now
  (==) (Reporting.Annotation.At r1 a) (Reporting.Annotation.At r2 b) = a == b

-- instance (Eq a) => Eq (Reporting.Annotation.Located a) where
--   (==) a b = show (Reporting.Annotation.At region a) = "(a (" ++ show a ++ "))"


deriving instance Show Reporting.Exit.DetailsBadDep
deriving instance Show Reporting.Exit.PackageProblem
deriving instance Show Reporting.Exit.Outline
deriving instance Show Reporting.Exit.OutlineProblem

deriving instance Show Reporting.Exit.Install
deriving instance Show Reporting.Exit.RegistryProblem
deriving instance Show Reporting.Exit.Solver
deriving instance Show Reporting.Exit.Details

deriving instance Show Elm.Version.Version

deriving instance Show Http.Error


instance Show Data.Name.Name where
  show = quoted . Data.Name.toChars

instance Show Elm.Package.Name where
  -- show = Elm.Package.toChars
  show (Elm.Package.Name author project) =
    "Name " ++ (quoted . Utf8.toChars) author <> " " <> (quoted . Utf8.toChars) project

instance Show Elm.Interface.Interface where
  show _ = "\"<Elm.Interface.Interface>\""

instance Show Elm.Interface.DependencyInterface where
  show _ = "\"<Elm.Interface.DependencyInterface>\""

instance Show Elm.Kernel.Chunk where
  show _ = "\"<Elm.Kernel.CHunk>\""

deriving instance Show Elm.Interface.Union
deriving instance Show Elm.Interface.Alias



deriving instance Show Elm.Constraint.Constraint

deriving instance Show Elm.Constraint.Error

deriving instance Show Elm.Outline.Outline
deriving instance Show Elm.Outline.AppOutline
deriving instance Show Elm.Outline.SrcDir
deriving instance Show Elm.Outline.PkgOutline
deriving instance Show Elm.Outline.Exposed

instance Show Elm.Licenses.License where
  show _ = "\"<Elm.Licenses.License>\""

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

instance Show (Elm.String.String) where
  show = quoted . Elm.String.toChars

-- deriving instance Show ModuleName.Canonical
instance Show ModuleName.Canonical where
  -- show (ModuleName.Canonical pkg moduleName) = quoted $ show pkg ++ ":" ++ Utf8.toChars moduleName
  show (ModuleName.Canonical pkg moduleName) = "(Module.Canonical (" ++ show pkg ++ ") " ++ show moduleName ++ ")"


instance (Show a) => Show (Data.NonEmptyList.List a) where
  show = show . Data.NonEmptyList.toList

instance Show Data.Index.ZeroBased where
  show (Data.Index.ZeroBased 0) = "Index.first"
  show (Data.Index.ZeroBased 1) = "Index.second"
  show (Data.Index.ZeroBased 2) = "Index.third"
  show (Data.Index.ZeroBased n) = "(Index.ZeroBased " ++ show n ++ ")"


deriving instance Show Type.Type.Constraint
deriving instance Show Type.Type.Type
deriving instance Show Type.Type.Variable
deriving instance (Show a) => Show (Reporting.Error.Type.Expected a)
deriving instance (Show a) => Show (Reporting.Error.Type.PExpected a)
deriving instance Show Reporting.Error.Type.Context
deriving instance Show Reporting.Error.Type.PContext
deriving instance Show Reporting.Error.Type.MaybeName
deriving instance Show Reporting.Error.Type.SubContext
deriving instance Show Reporting.Error.Type.Category
deriving instance Show Reporting.Error.Type.PCategory
deriving instance Show Type.Type.Descriptor
-- deriving instance (Show a) => Show (Type.UnionFind.Point a)
deriving instance Show Type.Type.Content
deriving instance Show Type.Type.SuperType
deriving instance Show Type.Type.FlatType
deriving instance Show Type.Type.Mark



instance Show (GHC.IORef.IORef a) where
  show _ = "<IORef>"


deriving instance Show Deps.Registry.KnownVersions



-- IsString

instance IsString Elm.Package.Project where
  fromString = Utf8.fromChars

instance IsString Elm.Package.Author where
  fromString = Utf8.fromChars

instance Show Json.String.String where
  show x = "\"" <> Utf8.toChars x <> "\""

instance IsString Json.String.String where
  fromString = Json.String.fromChars

instance IsString Elm.String.String where
  fromString = Utf8.fromChars





-- Helpers

showMapQualified :: (Show k, Show a) => Map.Map k a -> String
showMapQualified m =
  "Map.fromList " ++ show (Map.toList m)
