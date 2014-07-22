{-# LANGUAGE QuasiQuotes, RecordWildCards,  TemplateHaskell,TypeFamilies , NoImplicitPrelude
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager.Types.Acid  where 


import Prelude hiding (lookup)

-- Control
import Control.Applicative hiding (empty)
-- import Control.Monad
-- import Control.Lens

-- Acid

import Control.Monad.Reader ( ask )
import Control.Monad.State ( put , get)
import Data.SafeCopy (  base, deriveSafeCopy )
import Data.Acid
import Data.Text hiding (empty)

-- Containers
import Data.Map.Strict
import Data.ByteString hiding (empty)

-- Local
import Node.Manager.Client





newtype NodeManagerCellStore = NodeManagerCellStore {
  getNodes :: Map Text (NodeProcess ByteString)
}

initNodeManagerCellStore = NodeManagerCellStore { getNodes = empty}


$(deriveSafeCopy 0 'base ''CheckType)
$(deriveSafeCopy 0 'base ''KillMethod)
$(deriveSafeCopy 0 'base ''NodeProcess)
$(deriveSafeCopy 0 'base ''NodeManagerCellStore)





  
-- | digWith interface
type Name = Text

returnNodes :: Query NodeManagerCellStore (Map Text (NodeProcess ByteString))
returnNodes = getNodes <$> ask

getNode :: Name -> Query NodeManagerCellStore (Maybe (NodeProcess ByteString))
getNode name = do
  nodes <- getNodes <$> ask
  return $ lookup name nodes

insertNode :: (NodeProcess ByteString) ->  Update NodeManagerCellStore ()
insertNode node = do
  nodes <- getNodes <$> get  
  put (NodeManagerCellStore  (insert  (checkName node) node nodes ))

deleteNode :: Name -> Update NodeManagerCellStore ()
deleteNode name = do
  nodes <- getNodes <$> get
  put (NodeManagerCellStore (delete name nodes))

$(makeAcidic ''NodeManagerCellStore ['insertNode, 'deleteNode, 'getNode, 'returnNodes])
                                         
  
