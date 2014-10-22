{-# LANGUAGE OverloadedStrings #-}
module Node.Manager.RoutesSpec (routesSpec) where

import           TestImport

routesSpec :: Spec
routesSpec =  do
  ydescribe "rewriteRules" $
    yit "give a test rule and make sure it is rewritten to the expected value " $
      assertEqual "write and read values should work" (rewriteRules testRewriteTarget [testRewriteRule]) testRewriteResult
  ydescribe "makeKeyArr" $
    yit "takes a 'Value' and produces a [Vedit] this checks its encoding" $
      assertEqual "decoded from Value rewrite rule "  (makeKeyArr testEncodedRewriteRule)  [testRewriteRule]





