{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Node.Manager.Config  (  getHostPreference
                             , managerFilePath
                             , nodeManagerHost
                             , nodeManagerPort
                             , readNodeManagerConf
                             , MyHostPreference
                             , NodeManagerConfig
                            )where

-- General
import           Control.Monad
import qualified Data.ByteString                 as BS
import           Data.Streaming.Network.Internal
import           Data.Text
import           Data.Typeable
import qualified Filesystem.Path.CurrentOS       as OS
import           GHC.Generics
import           Prelude
-- Yaml
import           Data.Yaml

newtype MyHostPreference = MyHostPreference {
      getHostPreference :: HostPreference} deriving (Eq,Read,Show,Typeable,Generic)
instance FromJSON MyHostPreference where
    parseJSON (String str) = return . MyHostPreference . Host . unpack $ str
    parseJSON _ = fail "Parsing MyHostPreference Expected String, recieved Other"

instance ToJSON MyHostPreference where
    toJSON (MyHostPreference hp) = String $ pack $ show hp

data NodeManagerConfig = NodeManagerConfig {
      managerFilePath :: Text
    , nodeManagerHost :: MyHostPreference
    , nodeManagerPort :: Int
    } deriving (Read, Eq, Show, Typeable,Generic)

instance FromJSON NodeManagerConfig where

instance ToJSON NodeManagerConfig where

readNodeManagerConf :: OS.FilePath -> IO NodeManagerConfig
readNodeManagerConf fPath = do
	fCont <- BS.readFile (OS.encodeString fPath)
	either (\e -> fail e) (\asc -> return asc) $ decodeEither $ fCont
