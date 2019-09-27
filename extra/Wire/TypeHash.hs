{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Wire.TypeHash where

{- Hashes for Elm types
@TODO move into Evergreen namespace
-}

import qualified AST.Canonical as Can
import AST.Module.Name (Canonical(..))
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type


import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.List.Index (imap)
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Elm.Interface as Interface
import qualified Reporting.Progress as Progress

import qualified Data.ByteString.Char8 as BS8

import qualified File.IO as File

import CanSer.CanSer as CanSer

import Elm


possiblyWriteModelSha tell name elmi =
  -- @TODO be more careful and check module name too? i.e. elm/author
  if name == N.fromText "Backend" then
    case Map.lookup (N.fromText "Model") $ Interface._aliases elmi of
      Just tipe ->
        tell (Progress.LamderaWriteSha $ T.unpack $ hash tipe)

      Nothing ->
        pure ()
  else
    pure ()


hash showable =
  -- This won't be stable for changes to the AST. Does it matter? Can we just
  -- trust whatever SHA was generated with that version of the compiler?
  T.pack $ SHA.showDigest $ SHA.sha1 $ TLE.encodeUtf8 $ TL.pack $ show showable


write str =
  File.writeUtf8 ".lamdera-hash" (BS8.pack str)


modelHash :: Can.Module -> [(T.Text, T.Text)]
modelHash (Can.Module _moduName _docs _exports _decls _unions _aliases _binops _effects) =
  let

    hashVersion =
      -- @TODO think about this prefixing or something...
      "019-"


    aliasCodecs :: (N.Name, Can.Alias) -> (T.Text, T.Text)
    aliasCodecs (name, (Can.Alias names t)) =
      (N.toText name, hash t)
      -- Just $ "\ntype:" <> N.toText name <> "\nx:\n" <> (T.pack $ show _moduName) <> "\ntype canonical:\n" <> (T.pack $ show t) <> "\nhash:" <> hash t

  in
    ((aliasCodecs <$> Map.toList _aliases))
