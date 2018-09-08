-- Transpiler helpers/functions for Haskelm

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskelm.Core
  ( module Haskelm.Core
  -- Type classes
  , GHC.Show.Show(showsPrec)
  , Prelude.Ord
  , Prelude.Monoid
  , Prelude.Double
  , Prelude.Eq
  -- Helpers
  , Prelude.const
  , Prelude.showString
  , Prelude.pure
  , Prelude.error
  -- Re-exports
  , (<>) -- This is Haskell's equivalent of Elm's `++`
  , Prelude.fromIntegral
  , Vector
  , Aeson.Value(..)
  , T.unpack
  , T.pack
  -- SuperRecord re-exports
  , SuperRecord.rnil
  , SuperRecord.set
  , SuperRecord.get
  , (SuperRecord.&)
  , (SuperRecord.:=)(..)
  ) where

import GHC.Show (Show)
import Data.Monoid ((<>))
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Lazy as HML
import qualified Data.Hashable as H

import SuperRecord
import qualified SuperRecord as SR
import qualified Data.Scientific as Scientific

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as ATypes
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Char as Char
import Data.List (findIndices, isPrefixOf, tails)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- import GHC.Int (Int64)

show :: Show a => a -> T.Text
show x = T.pack (Prelude.show x)


p :: String -> IO ()
p = putStrLn

pt :: T.Text -> IO ()
pt = putStrLn . T.unpack

-- @TODO fix this up later
-- trace x a = DT.trace ("Debug.log: " <> Prelude.show x <> ": " <> Prelude.show a) a
-- {-# NOINLINE trace #-}
-- {-# RULES
-- "trace/Show a, Show b" identity' = identityString
-- "identity'/T.Text->Int->Int"    trace = traceInt
--
--   #-}


-- Re-export some "namespaced" SuperRecord stuff to avoid possible collisions with Elm code
type Record' a = SR.Record a


-- Helpers


-- A custom SuperRecord type operator that allows us to mimic the following Elm extensible-records type
--
-- type alias AbsoluteLength a = { a | someField : String }
--
-- i.e:
--
-- data type AbsoluteLength a = a .| '["someField" := String]

type family a .| b where
  Rec a .| b = Record (RecAppend a b)


-- "Native" re-exports
join :: T.Text -> [T.Text] -> T.Text
join = T.intercalate


-- Haskell realm helpers

-- These helpers assist with the glue we need to bridge some Haskell implementations into our
-- representational Elm world


-- Conversions from Haskell numerics to Haskelm
-- Currently we use Doubles to represent both Int/Floats, like Javascript does
-- The strange implementation here will make more sense if we need to change the types in future
toInt' :: Prelude.Integral a => a -> Prelude.Double
toInt' = Prelude.fromIntegral


toFloat' :: Prelude.Integral a => a -> Prelude.Double
toFloat' = Prelude.fromIntegral


asInt' :: Prelude.Double -> Prelude.Int
asInt' = Prelude.truncate


-- Map over any functor
fmap :: Prelude.Functor f => (a -> b) -> f a -> f b
fmap = Prelude.fmap


-- Extract a list of values from any Foldable value
foldableToList :: Foldable.Foldable t => t a -> [a]
foldableToList = Foldable.toList


listToArray :: [a] -> Vector a
listToArray = V.fromList


-- Note that this is unsafe, however Array in core does the bounds checking for us
-- Perhaps we should make it safe anyway and throw away the unsafe situation and log it?
arrayGet :: Prelude.Double -> Vector a -> a
arrayGet i arr = (V.!) arr (Prelude.truncate i)


arrayLength :: Vector a -> Prelude.Double
arrayLength v = Prelude.fromIntegral (V.length v)


arrayInitialize :: Prelude.Double -> (Prelude.Double -> a) -> Vector a
arrayInitialize int fn = V.generate (asInt' int) (fn . toInt')


arrayIndexedMap :: (Prelude.Double -> a -> b) -> Vector a -> Vector b
arrayIndexedMap fn v = V.imap (fn . toInt') v


-- The foldl fn arg in Haskell/Elm varies so we need to rearrange
-- foldl' :: (a -> b -> a) -> a -> Vector b -> a
-- foldl' :: (b -> a -> b) -> b -> Vector a -> b
-- foldl : (a -> b -> b) -> b -> Array a -> b
arrayFoldl :: (a -> b -> b) -> b -> Vector a -> b
arrayFoldl fn s v = V.foldl' (Prelude.flip fn) s v



-- For working with `Aeson.Object !Object`, where !Object is a Hashmap

hashToList :: HML.HashMap k v -> [(k, v)]
hashToList = HML.toList


hashLookup :: (Prelude.Eq k, H.Hashable k) => k -> HML.HashMap k v -> Prelude.Maybe v
hashLookup = HML.lookup


hashFromList :: (Prelude.Eq k, H.Hashable k) => [(k, v)] -> HML.HashMap k v
hashFromList = HML.fromList


-- Allows us to extract a Scientific representation used in `Aeson.Value(Number !Scientific)`
-- representation of a parsed number from JSON
-- See: http://hackage.haskell.org/package/aeson-1.2.3.0/docs/Data-Aeson-Types.html#t:Value
floatOrInt :: (Prelude.RealFloat r, Prelude.Integral i) => Scientific.Scientific -> Prelude.Either r i
floatOrInt = Scientific.floatingOrInteger


toScientific :: Prelude.RealFloat a => a -> Scientific.Scientific
toScientific = Scientific.fromFloatDigits


-- Aeson helpers

decode :: T.Text -> Prelude.Maybe Aeson.Value
decode text = Aeson.decode $ encodeUtf8 $ fromStrict text


encode :: ATypes.ToJSON a => a -> T.Text
encode val = toStrict $ decodeUtf8 $ Aeson.encode val



-- Core implementation helpers so we don't have to do stupid amounts of qualifications of Prelude collisions


-- https://www.stackage.org/haddock/lts-11.1/utility-ht-0.0.14/src/Data.List.HT.Private.html#search
search' :: T.Text -> T.Text -> [Double]
search' sub str = Prelude.fmap toInt' $ findIndices (isPrefixOf (T.unpack sub)) (tails (T.unpack str))


millisecondsSinceEpoch :: IO Double
millisecondsSinceEpoch = (toFloat' . Prelude.round . ((Prelude.*) 1000)) <$> getPOSIXTime
