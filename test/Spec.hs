{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec (hspec)
import           Yesod.Test
-- TestCases
import           TestImport
-- Specs
import           ConfigSpec (configSpec)
import           FileSpec   (fileSpec)

main :: IO ()
main =  do
   foundation <- mkTestFoundation
   hspec $
        yesodSpec foundation $ do
            configSpec
            fileSpec
