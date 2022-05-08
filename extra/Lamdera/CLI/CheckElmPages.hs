{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.CheckElmPages where

import Lamdera


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting elm-pages check..."

  root <- getProjectRoot

  pure ()
