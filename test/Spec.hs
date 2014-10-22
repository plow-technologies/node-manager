{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec              (hspec)
-- TestCases
import           TestImport
-- Specs
import           ConfigSpec              (configSpec)
import           Control.Concurrent      (forkIO, killThread)
import           FileSpec                (fileSpec)
import           Node.Manager            (defaultConfigStoredPath,
                                          initializeDirectory)
import           Node.Manager.RoutesSpec (routesSpec)

main :: IO ()
main =  do
   foundation <- mkTestFoundation
   tId <- forkIO $ warp 3001 foundation
   initializeDirectory defaultConfigStoredPath
   hspec $
        yesodSpec foundation $ do
            configSpec
            fileSpec
            routesSpec
   killThread tId

