{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec (hspec)
import           Yesod.Test
-- TestCases
import           TestImport
-- Specs
import           NodeSpec   (nodeSpec)

main :: IO ()
main =  do
   foundation <- mkFoundation
   hspec $ do
        yesodSpec foundation $ do
            nodeSpec
