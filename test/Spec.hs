{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec (hspec)
import           Yesod.Test
-- TestCases
import           TestImport
-- Specs
import           ConfigSpec (configSpec)

main :: IO ()
main =  do
   foundation <- mkTestFoundation
   hspec $ do
        yesodSpec foundation $ do
            configSpec
