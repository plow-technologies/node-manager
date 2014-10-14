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
import           Yesod
-- SimpleStore
import           Node.Manager.Types.SimpleStore (NodeManagerCellStore)
import           SimpleStore

data NodeManager = NodeManager {
    nodes :: SimpleStore NodeManagerCellStore
}

mkYesodData "NodeManager" $(parseRoutesFile "node-manager-routes")

