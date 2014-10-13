{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Node.Manager.Types (module Node.Manager.Types) where

import           Data.Aeson
import           Data.ByteString

import           Node.Manager.Client.Types      as Node.Manager.Types
import           Node.Manager.Routes.Foundation as Node.Manager.Types
import           Node.Manager.Types.SimpleStore as Node.Manager.Types
{-|
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

     }
                 deriving (Eq,Show,Generic,Typeable)
|-}

type ClientNodeProc = NodeProcess Value

type StorableNodeProc = NodeProcess ByteString
