{-# LANGUAGE QuasiQuotes, RecordWildCards, NoImplicitPrelude
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Node.Manager.Client.Types (NodeProcess (..)
                                 , KillMethod
                                 , CheckType
                                 , Vedit (..)
                                 ) where 

import Data.Text
import Data.Bool
import Prelude (Eq,Show,Int)
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Control.Applicative

data CheckType = GET | POST
               deriving (Eq,Show,Generic,Typeable)

data KillMethod = KillUrl Text | KillPID Int | KillNONE
                deriving (Eq,Show,Generic,Typeable)


-- | NodeProcess is parameterized in Body, so you can store it 
data NodeProcess v = NodeProcess {
           checkName :: Text
         , checkUrl :: Text
         , checkBody :: v
         , checkMethod :: CheckType
         , checkTime :: Int
         , checkKillMethod :: KillMethod
         
     } deriving (Eq,Show,Generic,Typeable)

data Vedit = Vedit {
           editKey :: Text
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

instance ToJSON CheckType where
instance FromJSON CheckType where   

  
instance ToJSON KillMethod where
instance FromJSON KillMethod where   

instance (ToJSON v) => ToJSON (NodeProcess v) where
instance (FromJSON v) => FromJSON (NodeProcess v) where   
