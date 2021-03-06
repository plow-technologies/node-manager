{-# LANGUAGE OverloadedStrings #-}

module TestImport
    (  module Yesod.Test
     , module Yesod.Core
     , module Node.Manager.Routes.Foundation
     , module Node.Manager.Routes
     , module Data.Aeson
     , module Class
     , Spec
     , Example
     , readTestConf
     , testRetriveRequest
     , testRetriveRequestWRewrite
     , testCopyRequest
     , testCloneRequest
     , testDeleteRequest
     , testCloneDirRequest
     , testRewriteTarget
     , testRewriteResult
     , testEncodedRewriteRule
     , testRewriteRule
     , mkTestFoundation
     ) where


import           Control.Monad.IO.Class         as Class
import           Data.Aeson                     (Value, decode, encode, object,
                                                 (.=))
import           Data.Maybe                     (fromJust)
import qualified Data.Yaml                      as Y (decodeFile)
import           Node.Manager.Client.Types      (Vedit (..))
import           Node.Manager.Routes
import           Node.Manager.Routes.Foundation
import           Node.Manager.Types.SimpleStore (initializeSimpleStore)
import           Yesod.Core                     (getYesod, warp)
import           Yesod.Test

mkTestFoundation :: IO NodeManager
mkTestFoundation = do
  nmcs <- initializeSimpleStore "NodeManagerStates"
  return NodeManager {nodes = nmcs}

readTestConf :: FilePath -> IO Value
readTestConf fPath = do
	mfCont <- Y.decodeFile fPath :: IO (Maybe Value)
        case mfCont of
             Nothing -> fail "Error: Reading Config file."
             Just fcont -> return fcont

testRetriveRequest :: Value
testRetriveRequest = object ["configName" .= ("alarm-state-config" :: String)]

testRetriveRequestWRewrite :: Value
testRetriveRequestWRewrite = object [ "configName" .= ("alarm-state-config"::String)
                                    , "rewrite-rules" .= [object ["key" .= ("host"::String)
                                                                , "val".= ("http://why not working.com"::String)] ]]

testCopyRequest :: Value
testCopyRequest = object ["route".=("http://127.0.0.1:3001/configure/add"::String)]

testRewriteTarget :: Value
testRewriteTarget = object ["config" .= object ["testrule" .= mormon]]
  where
    mormon :: String
    mormon = "mormon"

testRewriteResult :: Value
testRewriteResult = object ["config" .= object ["testrule" .= mormon]]
  where
    mormon :: String
    mormon = "hope floats"

testRewriteRule :: Vedit
testRewriteRule = Vedit "testrule" "hope floats"

testEncodedRewriteRule :: Value
testEncodedRewriteRule = fromJust $ decode "{\"rewrite-rules\":[{\"key\":\"testrule\",\"val\":\"hope floats\"}]}"


cloneList :: [String]
cloneList = ["alarm-state-config"]

testCloneRequest :: Value
testCloneRequest = object ["route".=("http://127.0.0.1:3001/configure/add"::String), "nameList" .= cloneList]

testDeleteRequest :: String
testDeleteRequest = "alarm-state-config"

testCloneDirRequest :: Value
testCloneDirRequest = object ["directoryName" .= ("testConfigsDir"::String)]

type Spec = YesodSpec NodeManager
type Example = YesodExample NodeManager

