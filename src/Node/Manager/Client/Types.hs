{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}


module Node.Manager.Client.Types (NodeProcess (..)
                                 , KillMethod
                                 , CheckType
                                 , Vedit (..)
                                 ) where

import           Control.Applicative
import           Control.Monad       (fail)
import           Data.Aeson
import           Data.Serialize
import           Data.Text
import           Data.Text.Encoding
import           Data.Typeable
import           GHC.Generics
import           Prelude             (Eq, Int, Show, fmap, ($))

data CheckType = GET | POST
               deriving (Eq,Show,Generic,Typeable)

data KillMethod = KillUrl Text | KillPID Int | KillNONE
                deriving (Eq,Show,Generic,Typeable)


-- | NodeProcess is parameterized in Body, so you can store it
data NodeProcess v = NodeProcess {
           checkName       :: Text
         , checkUrl        :: Text
         , checkBody       :: v
         , checkMethod     :: CheckType
         , checkTime       :: Int
         , checkKillMethod :: KillMethod

     } deriving (Eq,Show,Generic,Typeable)

instance (Serialize v) =>  Serialize (NodeProcess v) where

data Vedit = Vedit {
           editKey   :: Text
         , editValue :: Value
} deriving (Eq, Show, Generic, Typeable)


instance ToJSON Vedit where
  toJSON (Vedit {editKey = k
               , editValue = v}) = object ["key" .= k
                                         , "val" .= v]

instance FromJSON Vedit where
  parseJSON (Object o) = Vedit <$>
                         o .: "key" <*>
                         o .: "val"
  parseJSON _ = fail "Rule: Expecting Vedit Object Received, Other"

instance ToJSON CheckType where
instance FromJSON CheckType where

instance Serialize CheckType where


instance ToJSON KillMethod where
instance FromJSON KillMethod where

instance Serialize KillMethod where

instance Serialize Text where
 put txt = put $ encodeUtf8 txt
 get     = fmap decodeUtf8 get

instance (ToJSON v) => ToJSON (NodeProcess v) where
instance (FromJSON v) => FromJSON (NodeProcess v) where
