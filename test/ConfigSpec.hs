{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (configSpec) where

import           Control.Monad.Trans.Either
import qualified Data.Yaml                  as Y (decodeFile)
import           Node.Manager.Client
import           Test.Hspec
import           Yesod.Core

-- configSpec :: Spec
-- configSpec =
--  ydescribe "postAddConfigureR" $ do
--     yit "Parses a config and inserts it into the manager" $ do
--       testNodeConfig <- liftIO $ readTestConf "testConfigs/testAddConfig.yml"
--       postBody AddConfigureR (encode testNodeConfig)
--       printBody >> statusIs 200

--   ydescribe "getConfigureR" $
--     yit "Return all the Config Files in the Node Manager" $ do
--             get ConfigureR
--             printBody >> statusIs 200

--   ydescribe "postEditConfigureR" $ do
--     yit "Return a config using configName, no rewriteRule" $ do
--             postBody EditConfigureR (encode testRetriveRequest)
--             statusIs 200

--     yit "Return a config using configName, using rewriteRule" $ do
--             postBody EditConfigureR (encode testRetriveRequestWRewrite)
--             bodyContains "why not working.com"
--             statusIs 200

--   ydescribe "postCopyConfigureR" $
--     yit "Requests the copies of all the configs from the node manager" $ do
--             postBody CopyConfigureR (encode testCopyRequest)
--             printBody >> statusIs 200

--   ydescribe "postDeleteConfigureR" $
--     yit "Delete a configure using a configure name" $ do
--             postBody DeleteConfigureR (encode testDeleteRequest)
--             printBody >> statusIs 200
-- fileSpec :: Spec
-- fileSpec =  ydescribe "postCloneDirtoryR" $
--   yit "Clone a diretory, and ensure the common config files can save to Node Manager" $ do
--     postBody CloneDiretoryR (encode testCloneDirRequest)
--     printBody >> statusIs 200
-- routesSpec :: Spec
-- routesSpec =  do
--   ydescribe "rewriteRules" $
--     yit "give a test rule and make sure it is rewritten to the expected value " $
--       assertEqual "write and read values should work" (rewriteRules testRewriteTarget [testRewriteRule]) testRewriteResult
--   ydescribe "makeKeyArr" $
--     yit "takes a 'Value' and produces a [Vedit] this checks its encoding" $
--       assertEqual "decoded from Value rewrite rule "  (makeKeyArr testEncodedRewriteRule)  [testRewriteRule]

readTestConf :: FilePath -> IO Value
readTestConf fPath = do
        mfCont <- Y.decodeFile fPath :: IO (Maybe Value)
        case mfCont of
             Nothing -> fail "Error: Reading Config file."
             Just fcont -> return fcont

configSpec :: Spec
configSpec = do
  -- describe "/configure/add"
  --   it "Parses a config and inserts it into the manager" $ do
  --     testNodeConfig <- liftIO $ readTestConf "testConfigs/testAddConfig.yml"
  --     addConfig testNodeConfig
  describe "/get/configure" $
    it "Returns all config files in node manager" $ do
      eConf <- runEitherT getConfig
      let val = case eConf of
                  Right _ -> True
                  _ -> False
      val `shouldBe` True
