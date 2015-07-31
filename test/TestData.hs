{-# LANGUAGE OverloadedStrings #-}

module TestData
    (  readTestConf
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
     ) where


import           Control.Monad.IO.Class as Class
import           Data.Aeson             (Value, decode, encode, object, toJSON,
                                         (.=))
import           Data.Maybe             (fromJust)
import qualified Data.Yaml              as Y (decodeFile)
import           Node.Manager.Routes
import           Node.Manager.Types     (Vedit (..))
import           Test.Hspec
import           Yesod.Core             (getYesod, warp)
import           Yesod.Test


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


cloneList :: Value
cloneList = toJSON (["alarm-state-config"] :: [String])

testCloneRequest :: Value
testCloneRequest = object ["route".=("http://127.0.0.1:3001/configure/add"::String), "nameList" .= cloneList]

testDeleteRequest :: Value
testDeleteRequest = toJSON ("alarm-state-config" :: String)

testCloneDirRequest :: Value
testCloneDirRequest = object ["directoryName" .= ("testConfigsDir"::String)]
