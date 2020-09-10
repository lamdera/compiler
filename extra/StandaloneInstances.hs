{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StandaloneInstances where

import Data.String (IsString, fromString)

import qualified Data.Utf8 as Utf8
import qualified AST.Optimized
import qualified Data.Name
import qualified Elm.Package
import qualified Elm.ModuleName as ModuleName
import qualified Json.String


-- import qualified AST.Canonical as Can
-- import qualified AST.Source as Src
-- import qualified Elm.Interface as Interface
-- import qualified AST.Utils.Binop as Binop
-- import qualified Data.Index as Index -- from elm compiler
-- import qualified File.Find as Find
-- import qualified "elm" Reporting.Annotation as Ann



-- Show

deriving instance Show AST.Optimized.Global

instance Show Data.Name.Name where
  show = Data.Name.toChars

instance Show Elm.Package.Name where
  show = Elm.Package.toChars

instance Show ModuleName.Canonical where
  show (ModuleName.Canonical pkg moduleName) = show pkg ++ ":" ++ show moduleName


-- IsString

instance IsString Elm.Package.Project where
  fromString = Utf8.fromChars

instance IsString Elm.Package.Author where
  fromString = Utf8.fromChars

instance IsString Json.String.String where
  fromString = Json.String.fromChars



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
