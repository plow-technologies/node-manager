{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where


import           Test.Hspec         (hspec)
import           Yesod.Test
-- TestCases
import           TestImport
-- Specs
import           ConfigSpec         (configSpec)
import           Control.Concurrent (forkIO, killThread)
import           FileSpec           (fileSpec)
import           Node.Manager       (defaultConfigStoredPath,
                                     initializeDirectory)
import           Yesod              (warp)

main :: IO ()
main =  do
   foundation <- mkTestFoundation
   tId <- forkIO $ warp 3001 foundation
   initializeDirectory defaultConfigStoredPath
   hspec $
        yesodSpec foundation $ do
            configSpec
            fileSpec
   killThread tId
