{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Node.Manager.Config  (  readNodeManagerConf
                             , MyHostPreference (..)
                             , NodeManagerConfig (..)
                            )where

-- General
import qualified Data.ByteString                 as BS (readFile)
import           Data.Streaming.Network.Internal (HostPreference (..))
import           Data.Text                       (Text, pack, unpack)
import           Data.Typeable                   (Typeable)
import qualified Filesystem.Path.CurrentOS       as OS (FilePath, encodeString)
import           GHC.Generics                    (Generic)

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
	either fail return $ decodeEither fCont
