{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager.Types (Vedit (..)) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON, ToJSON, Value (Object), object,
                                      parseJSON, toJSON, (.:), (.=))
import           Data.Serialize      (Serialize, get, put)
import           Data.Text           (Text)
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)


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

instance Serialize Text where
 put txt = put $ encodeUtf8 txt
 get     = fmap decodeUtf8 get
