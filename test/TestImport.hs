{-# LANGUAGE OverloadedStrings #-}

module TestImport
    (  module Yesod.Test
     , module Yesod
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
     ) where


import           Control.Monad.IO.Class         as Class
import           Data.Aeson
import           Node.Manager.Routes
import           Node.Manager.Routes.Foundation
import           Yesod                          (getYesod)
import           Yesod.Test

import qualified Data.Yaml                      as Y

readTestConf :: FilePath -> IO Value
readTestConf fPath = do
	mfCont <- Y.decodeFile fPath :: IO (Maybe Value)
        case mfCont of
             Nothing -> fail "Error: Reading Config file."
             Just fcont -> return fcont

testRetriveRequest :: Value
testRetriveRequest = object ["configName" .= ("alarm-state-config" :: String)]

testRetriveRequestWRewrite :: Value
testRetriveRequestWRewrite = object ["configName" .= ("alarm-state-config"::String) , "rewrite-rules" .= object ["key" .= ("port"::String) , "val".= (2::Int)]]

testCopyRequest :: Value
testCopyRequest = object ["route".=("http://127.0.0.1:3000/configure/add"::String)]

cloneList :: [String]
cloneList = ["alarm-state-config"]

testCloneRequest :: Value
testCloneRequest = object ["route".=("http://127.0.0.1:3000/configure/add"::String), "nameList" .= cloneList]

testDeleteRequest :: String
testDeleteRequest = "alarm-state-config"


type Spec = YesodSpec NodeManager
type Example = YesodExample NodeManager

