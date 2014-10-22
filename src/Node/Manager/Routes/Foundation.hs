{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes.Foundation  ( module Node.Manager.Routes.Foundation
                                       ) where
import           Yesod.Core                     (renderRoute)
import           Yesod.Core.Dispatch            (mkYesodData, parseRoutesFile)
-- SimpleStore
import           Node.Manager.Types.SimpleStore (NodeManagerCellStore)
import           SimpleStore                    (SimpleStore)

data NodeManager = NodeManager {
    nodes :: SimpleStore NodeManagerCellStore
}

mkYesodData "NodeManager" $(parseRoutesFile "node-manager-routes")

