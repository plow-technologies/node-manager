{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes.Foundation  where
import           Prelude
import           Yesod

-- SimpleStore
import           SimpleStore

import           Node.Manager.Types.Acid

data NodeManager = NodeManager {
    nodes :: (SimpleStore NodeManagerCellStore)
}

mkYesodData "NodeManager" $(parseRoutesFile "node-manager-routes")


mkFoundation :: IO NodeManager
mkFoundation = do
  nmcs <- initializeSimpleStore "NodeManagerState"
  return $ NodeManager {nodes = nmcs}
