{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Node.Manager.Types.SimpleStore  (NodeManagerCellStore (..), initNodeManagerCellStore
                                       , initializeSimpleStore, Name, returnNodes, getNode, insertNode,deleteNode ) where

-- General
import           GHC.Generics              (Generic)
-- Serialization
import           Data.Serialize            (Serialize)
import           Data.Text                 (Text)
-- Containers
import           Data.ByteString           (ByteString)
import qualified Data.Map.Strict           as M (Map, delete, empty, insert,
                                                 lookup)
-- Local
import qualified Filesystem.Path.CurrentOS as OS (FilePath)
import           Node.Manager.Client       (NodeProcess, checkName)
import           SimpleStore               (SimpleStore (SimpleStore),
                                            getSimpleStore, makeSimpleStore,
                                            putSimpleStore)

newtype NodeManagerCellStore = NodeManagerCellStore {
  getNodes :: M.Map Text (NodeProcess ByteString)
} deriving (Generic)

instance Serialize NodeManagerCellStore where

initNodeManagerCellStore :: NodeManagerCellStore
initNodeManagerCellStore = NodeManagerCellStore { getNodes = M.empty}

initializeSimpleStore :: OS.FilePath -> IO (SimpleStore NodeManagerCellStore)
initializeSimpleStore fpr = do
   enmCellStore <- makeSimpleStore fpr initNodeManagerCellStore
   either (\_ -> fail "SimpleStore won't initialize" ) return  enmCellStore

-- | digWith interface
type Name = Text

returnNodes :: SimpleStore st -> IO st
returnNodes = getSimpleStore

getNode :: Ord k => SimpleStore (M.Map k a) -> k -> IO (Maybe a)
getNode st name = do
     nodes <- getSimpleStore st
     return $ M.lookup name nodes

insertNode :: SimpleStore NodeManagerCellStore -> NodeProcess ByteString -> IO ()
insertNode st node = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (M.insert (checkName node) node (getNodes nodes)))

deleteNode :: SimpleStore NodeManagerCellStore -> Text -> IO ()
deleteNode st name = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (M.delete name (getNodes nodes)))




