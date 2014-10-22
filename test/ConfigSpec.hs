{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (configSpec) where

import           TestImport

configSpec :: Spec
configSpec =  ydescribe "postAddConfigureR" $ do
  yit "Parses an config and insert it into the manager" $ do
    testNodeConfig <- liftIO $ readTestConf "testConfigs/testAddConfig.yml"
    postBody AddConfigureR (encode testNodeConfig)
    printBody >> statusIs 200

  ydescribe "getConfigureR" $
    yit "Return all the Config Files in the Node Manager" $ do
            get ConfigureR
            printBody >> statusIs 200

  ydescribe "postEditConfigureR" $ do
    yit "Return a config using configName, no rewriteRule" $ do
            postBody EditConfigureR (encode testRetriveRequest)
            statusIs 200

    yit "Return a config using configName, using rewriteRule" $ do
            postBody EditConfigureR (encode testRetriveRequestWRewrite)
            bodyContains "why not working.com"
            statusIs 200

  ydescribe "postCopyConfigureR" $
    yit "Requests the copies of all the configs from the node manager" $ do
            postBody CopyConfigureR (encode testCopyRequest)
            printBody >> statusIs 200

  ydescribe "postDeleteConfigureR" $
    yit "Delete a configure using a configure name" $ do
            postBody DeleteConfigureR (encode testDeleteRequest)
            printBody >> statusIs 200





