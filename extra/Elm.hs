module Elm
  ( DT.trace
  , PP.sShow
  , PP.tShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  , List
  , ppElm
  , Data.Witherable.mapMaybe
  , Data.Witherable.catMaybes
  )
  where

-- A prelude-like thing that contains the commonly used things in elm.
-- Names differ, but semantics are similar.

import Prelude ()

import qualified Debug.Trace as DT
import qualified Wire.PrettyPrint as PP
import qualified Data.Text as T
import Data.Monoid ((<>), mconcat)
import Data.Function ((&))
import Control.Arrow (first, second)

import CanSer.CanSer (ppElm)
import qualified Data.Witherable

type List a = [a]
