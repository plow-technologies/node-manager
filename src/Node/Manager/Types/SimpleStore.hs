{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager.Types.SimpleStore  where

-- General
import           GHC.Generics
import           Prelude                   hiding (FilePath, lookup)
-- Serialization
import           Data.Serialize
import           Data.Text                 hiding (empty)
-- Containers
import           Data.ByteString           hiding (empty)
import           Data.Map.Strict
-- Local
import           Filesystem.Path.CurrentOS (FilePath)
import           Node.Manager.Client
import           SimpleStore

newtype NodeManagerCellStore = NodeManagerCellStore {
  getNodes :: Map Text (NodeProcess ByteString)
} deriving (Generic)

instance Serialize NodeManagerCellStore where

initNodeManagerCellStore :: NodeManagerCellStore
initNodeManagerCellStore = NodeManagerCellStore { getNodes = empty}

initializeSimpleStore ::FilePath -> IO (SimpleStore NodeManagerCellStore)
initializeSimpleStore fpr = do
   enmCellStore <- makeSimpleStore fpr initNodeManagerCellStore
   either (\_ -> fail "SimpleStore won't initialize" ) return  enmCellStore

-- | digWith interface
type Name = Text

returnNodes :: SimpleStore st -> IO st
returnNodes = getSimpleStore

getNode :: Ord k => SimpleStore (Map k a) -> k -> IO (Maybe a)
getNode st name = do
     nodes <- getSimpleStore st
     return $ lookup name nodes

insertNode :: SimpleStore NodeManagerCellStore -> NodeProcess ByteString -> IO ()
insertNode st node = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (insert (checkName node) node (getNodes nodes)))

deleteNode :: SimpleStore NodeManagerCellStore -> Text -> IO ()
deleteNode st name = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (delete name (getNodes nodes)))




