{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec (hspec)
import           TestSpecs  (configSpec, fileSpec, routesSpec)

main :: IO ()
main =  hspec $ do
  configSpec
  fileSpec
  routesSpec
