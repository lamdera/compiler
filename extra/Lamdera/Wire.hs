{-# LANGUAGE OverloadedStrings #-}
module Lamdera.Wire where

import Elm.Package
import StandaloneInstances


shouldHaveCodecsGenerated :: Name -> Bool
shouldHaveCodecsGenerated name =
  case name of
    -- Some elm packages are ignored because of cyclic dependencies.
    -- Those codecs have to be manually defined in `lamdera/codecs`.
    -- All other packages, even if their types are defined in js, have codecs generated for their types.
    -- Then we manually override specific types in `Wire.Source`.

    -- elm deps used by lamdera/codecs
    Name "elm" "bytes" -> False
    Name "elm" "core" -> False

    Name "lamdera" "codecs" ->
      -- avoid cyclic imports; generated codecs rely on lamdera/codecs:Lamdera.Wire. This is our codec bootstrap module.
      False

    _ ->
      True
