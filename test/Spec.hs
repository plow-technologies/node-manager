{-# LANGUAGE OverloadedStrings #-}

module Main where


import           ConfigSpec         (configSpec)
import           Control.Concurrent (forkIO, killThread)
import           Node.Manager       (defaultConfigStoredPath,
                                     initializeDirectory)
import           Test.Hspec         (hspec)

main :: IO ()
main =  undefined
