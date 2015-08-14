{-# LANGUAGE OverloadedStrings #-}
module TestSpecs (configSpec, fileSpec, routesSpec) where

import           Control.Monad.Trans.Either
import           Node.Manager.Client
import           Node.Manager.Routes
import           Test.Hspec
import           TestData

configSpec :: Spec
configSpec = do
  describe "/configure/add" $
    it "Parses a config and inserts it into the manager" $ do
      testConfig <- readTestConf "testConfigs/testAddConfig.yml"
      eResult <- runEitherT $ addConfig testConfig
      let passed = case eResult of
                     Right _ -> True
                     _       -> False
      passed `shouldBe` True

  describe "/configure/get" $
    it "Returns all config files in node manager" $ do
      eResult <- runEitherT getConfig
      let passed = case eResult of
                     Right _ -> True
                     _       -> False
      passed `shouldBe` True

  describe "/configure/edit --No Rewrite" $
    it "Returns a config using configName, no rewriteRule" $ do
    eResult <- runEitherT $ retrieveConfig testRetriveRequest
    let passed = case eResult of
                   Right _ -> True
                   _       -> False
    passed `shouldBe` True

  describe "/configure/edit --Rewrite" $
    it "Returns a config using configName, using rewriteRule" $ do
    eResult <- runEitherT $ retrieveConfig testRetriveRequestWRewrite
    let passed = case eResult of
                   Right _ -> True
                   _       -> False
    passed `shouldBe` True


  describe "/configure/delete" $
    it "Deletes a configure using a configure name" $ do
    eResult <- runEitherT $ deleteConfig testDeleteRequest
    let passed = case eResult of
                   Right _ -> True
                   _       -> False
    passed `shouldBe` True

  describe "/configure/copy" $
    it "Requests the copies of all the configs from the node manager" $ do
    eResult <- runEitherT $ deleteConfig testCopyRequest
    let passed = case eResult of
                   Right _ -> True
                   _       -> False
    passed `shouldBe` True

fileSpec :: Spec
fileSpec =
  describe "/clone" $
    it "Clone a diretory, and ensure the common config files can save to Node Manager" $ do
    eResult <- runEitherT $ cloneDirectory testCloneDirRequest
    let passed = case eResult of
                   Right _ -> True
                   _       -> False
    passed `shouldBe` True

routesSpec :: Spec
routesSpec =  do
  describe "rewriteRules" $
   it "give a test rule and make sure it is rewritten to the expected value " $
    testRewriteResult `shouldBe` rewriteRules testRewriteTarget [testRewriteRule]
  describe "makeKeyArr" $
   it "takes a 'Value' and produces a [Vedit] this checks its encoding" $
    [testRewriteRule] `shouldBe` makeKeyArr testEncodedRewriteRule