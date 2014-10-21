{-# LANGUAGE OverloadedStrings #-}
module Node.Manager.RoutesSpec (routesSpec) where

-- import Node.Manager.Routes (rewriteRules)
import TestImport 

routesSpec :: Spec
routesSpec =  do
  ydescribe "rewriteRules" $ do
    yit "give a test rule and make sure it is rewritten to the expected value " $ do
      assertEqual "write and read values should work" (rewriteRules testRewriteTarget [testRewriteRule]) (testRewriteResult)
  ydescribe "makeKeyArr" $ do
    yit "takes a 'Value' and produces a [Vedit] this checks its encoding" $ do
      assertEqual "decoded from Value rewrite rule "  (makeKeyArr testEncodedRewriteRule)  [testRewriteRule]
-- rewriteRules :: Value -> [Vedit] -> Value
    


    
