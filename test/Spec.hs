{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec   (hspec)
import           Yesod.Test
-- TestCases
import           TestImport
-- Specs
import           ConfigSpec   (configSpec)
import           FileSpec     (fileSpec)
import           Node.Manager (defaultConfigStoredPath, initializeDirectory)

main :: IO ()
main =  do
   foundation <- mkTestFoundation
   initializeDirectory defaultConfigStoredPath
   hspec $
        yesodSpec foundation $ do
            configSpec
            fileSpec
