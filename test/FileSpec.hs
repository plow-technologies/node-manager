{-# LANGUAGE OverloadedStrings #-}
module FileSpec (fileSpec) where

import           TestImport

fileSpec :: Spec
fileSpec =  ydescribe "postCloneDirtoryR" $
  yit "Clone a diretory, and ensure the common config files can save to Node Manager" $ do
    postBody CloneDiretoryR (encode testCloneDirRequest)
    printBody >> statusIs 200
