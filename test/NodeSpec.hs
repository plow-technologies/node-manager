{-# LANGUAGE OverloadedStrings #-}
module NodeSpec (nodeSpec) where

import           TestImport

nodeSpec :: Spec
nodeSpec =  ydescribe "postAddConfigureR" $ do
  yit "Parses an config and insert it into the manager" $ do
    testNodeConfig <- liftIO $ readTestConf "testConfigs/testAddConfig.yml"
    postBody AddConfigureR (encode testNodeConfig)
    printBody >> statusIs 200

  ydescribe "postEditConfigureR" $ do
    yit "Return a config using configName, no rewriteRule" $ do
            postBody EditConfigureR (encode testRetriveRequest)
            printBody >> statusIs 200

    yit "Return a config using configName, no rewriteRule" $ do
            postBody EditConfigureR (encode testRetriveRequestWRewrite)
            printBody >> statusIs 200
  ydescribe "postCopyConfigureR" $ -- only test if there is another warp running
    yit "Requests the copies of all the configs from the node manager" $ do
            return ()
            -- postBody CopyConfigureR (encode testCopyRequest)
            -- printBody >> statusIs 200
  ydescribe "postCloneConfigureR" $ -- only test if there is another warp running
    yit "Requests the copies of all the configs from the node manager" $ do
            return ()
            -- postBody CopyConfigureR (encode testCloneRequest)
            -- printBody >> statusIs 200
  -- ydescribe "postDeleteConfigureR" $
  --   yit "Delete a configure using a configure name" $ do
  --           postBody DeleteConfigureR (encode testDeleteRequest)
  --           printBody >> statusIs 200





