{-# LANGUAGE CPP, OverloadedStrings #-}

module Lamdera.Filewatch where

#ifdef OS_Mac
import qualified Lamdera.Filewatch_Brute as Filewatch
#else
import qualified Lamdera.Filewatch_FSNotify as Filewatch
#endif

watch = Filewatch.watch
