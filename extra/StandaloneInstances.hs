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

instance Show Elm.Float.Float where
  show = T.unpack . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString . Elm.Float.toBuilder

deriving instance (Show a) => Show (Json.Decode.Error a)
deriving instance (Show a) => Show (Json.Decode.Problem a)
deriving instance Show (Json.Decode.ParseError)
deriving instance Show (Json.Decode.StringProblem)
deriving instance Show (Json.Decode.DecodeExpectation)
deriving instance Show Reporting.Annotation.Region
deriving instance Show Reporting.Annotation.Position
deriving instance (Show a) => Show (Reporting.Annotation.Located a)

deriving instance Show Reporting.Exit.DetailsBadDep
deriving instance Show Reporting.Exit.PackageProblem
deriving instance Show Reporting.Exit.Outline
deriving instance Show Reporting.Exit.OutlineProblem
deriving instance Show Elm.Version.Version

deriving instance Show Http.Error


instance Show Data.Name.Name where
  show = Data.Name.toChars

instance Show Elm.Package.Name where
  show = Elm.Package.toChars

instance Show Elm.Interface.Interface where
  show _ = "\"<Elm.Interface.Interface>\""

instance Show Elm.Interface.DependencyInterface where
  show _ = "\"<Elm.Interface.DependencyInterface>\""

instance Show Elm.Kernel.Chunk where
  show _ = "\"<Elm.Kernel.CHunk>\""


deriving instance Show Elm.Constraint.Constraint

deriving instance Show Elm.Constraint.Error

deriving instance Show Elm.Outline.Outline
deriving instance Show Elm.Outline.AppOutline
deriving instance Show Elm.Outline.SrcDir
deriving instance Show Elm.Outline.PkgOutline
deriving instance Show Elm.Outline.Exposed

instance Show Elm.Licenses.License where
  show _ = "\"<Elm.Licenses.License>\""




instance Show (Elm.String.String) where
  show = Elm.String.toChars

instance Show ModuleName.Canonical where
  show (ModuleName.Canonical pkg moduleName) = show pkg ++ ":" ++ show moduleName

-- IsString

instance IsString Elm.Package.Project where
  fromString = Utf8.fromChars

instance IsString Elm.Package.Author where
  fromString = Utf8.fromChars


instance Show Json.String.String where
  show x = "\"" <> Utf8.toChars x <> "\""

instance IsString Json.String.String where
  fromString = Json.String.fromChars


instance (Show a) => Show (Data.NonEmptyList.List a) where
  show = show . Data.NonEmptyList.toList




-- instance Show Elm.Name.Name where
--   show = Elm.Name.toString
--
-- instance Show Elm.Package.Version where
--   show = Elm.Package.versionToString
--
--
-- instance Show Elm.Package.Package where
--   show (Elm.Package.Package name version) = show name ++ ":" ++ show version
--
-- deriving instance Show Can.Alias
--
-- deriving instance Show Can.Type
--
--
-- instance Show Can.FieldType where
--   show (Can.FieldType _ t) = show t
--
-- deriving instance Show Can.AliasType
-- deriving instance Show Can.Annotation
-- deriving instance Show Can.Union
-- deriving instance Show Can.Ctor
-- deriving instance Show Can.CtorOpts
--
-- deriving instance Show Interface.Union
-- deriving instance Show Interface.Alias
-- deriving instance Show Interface.Binop
-- deriving instance Show Interface.Interface
-- deriving instance Show Binop.Precedence
-- deriving instance Show Binop.Associativity
--
-- instance Show Index.ZeroBased where
--   show (Index.ZeroBased x) = show x
--
-- deriving instance Show Find.Asset
--
-- deriving instance Show Src.Exposing
-- deriving instance Show Src.Exposed
-- deriving instance Show Src.Privacy
-- deriving instance Show Src.Import

-- instance Show a => Show (Ann.Located a) where
--    show = show . Ann.toValue
--
-- deriving instance Functor Ann.Located
-- deriving instance Foldable Ann.Located
-- deriving instance Traversable Ann.Located


-- instance Show Global where
--   show (Global can name) = show can ++ "." ++ show name


-- instance Show Name where
  -- show n = Text.unpack $ toText n

-- instance Show Name where
--   show (Name toBuilder) = "Name<" ++ show (B.toLazyByteString toBuilder) ++ ">"


-- instance Show a => Show (Point a) where
--   show (Pt a) =
--     unsafePerformIO $ do
--       v <- readIORef a
--       pure ("<Pt:" ++ show v ++ ">")
--
-- instance Show a => Show (PointInfo a) where
--   show (Info w32 a) =
--     unsafePerformIO $ do
--       w32r <- readIORef w32
--       a' <- readIORef a
--       pure ("Info " ++ show w32r ++ " " ++ show a')
--   show (Link a) =
--     "Link (" ++ show a ++ ")"
