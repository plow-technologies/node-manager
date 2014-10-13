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
   fAcidSt <- (openSimpleStore fpr) >>= (either (\_ -> do
                                                        enmCellStore <- (makeSimpleStore fpr initNodeManagerCellStore)
                                                        either (\_ -> fail "SimpleStore won't initialize" ) (return ) enmCellStore
                                                    ) (return) ) ::  IO (SimpleStore NodeManagerCellStore)
   return fAcidSt

-- | digWith interface
type Name = Text

returnNodes = getSimpleStore

getNode st name = do
     nodes <- getSimpleStore st
     return $ lookup name nodes

insertNode st node = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (insert (checkName node) node (getNodes nodes)))

deleteNode st name = do
     nodes <- getSimpleStore st
     putSimpleStore st (NodeManagerCellStore (delete name (getNodes nodes)))




