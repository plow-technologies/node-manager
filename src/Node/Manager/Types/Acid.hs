{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager.Types.Acid  where


import           Data.Serialize
import           GHC.Generics
import           Prelude              hiding (lookup)
-- Control
import           Control.Applicative  hiding (empty)

-- Acid

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Text            hiding (empty)

-- Containers
import           Data.ByteString      hiding (empty)
import           Data.Map.Strict

-- Local
import           Node.Manager.Client
import           SimpleStore



newtype NodeManagerCellStore = NodeManagerCellStore {
  getNodes :: Map Text (NodeProcess ByteString)
} deriving (Generic)

instance Serialize NodeManagerCellStore where


initNodeManagerCellStore = NodeManagerCellStore { getNodes = empty}

initializeSimpleStore fpr = do
   fAcidSt <- (openSimpleStore ( fpr )) >>= (either (\_ -> do
                                                        enmCellStore <- (makeSimpleStore fpr initNodeManagerCellStore)
                                                        either (\_ -> fail "SimpleStore won't initialize" ) (return ) enmCellStore
                                                    ) (return) ) ::  IO (SimpleStore NodeManagerCellStore)
   return fAcidSt

-- (deriveSafeCopy 0 'base ''CheckType)
-- $(deriveSafeCopy 0 'base ''KillMethod)
-- $(deriveSafeCopy 0 'base ''NodeProcess)
-- $(deriveSafeCopy 0 'base ''NodeManagerCellStore)






-- -- | digWith interface
-- type Name = Text

-- returnNodes :: Query NodeManagerCellStore (Map Text (NodeProcess ByteString))
-- returnNodes = getNodes <$> ask

-- getNode :: Name -> Query NodeManagerCellStore (Maybe (NodeProcess ByteString))
-- getNode name = do
--   nodes <- getNodes <$> ask
--   return $ lookup name nodes

-- insertNode :: (NodeProcess ByteString) ->  Update NodeManagerCellStore ()
-- insertNode node = do
--   nodes <- getNodes <$> get
--   put (NodeManagerCellStore  (insert  (checkName node) node nodes ))

-- deleteNode :: Name -> Update NodeManagerCellStore ()
-- deleteNode name = do
--   nodes <- getNodes <$> get
--   put (NodeManagerCellStore (delete name nodes))

-- $(makeAcidic ''NodeManagerCellStore ['insertNode, 'deleteNode, 'getNode, 'returnNodes])


