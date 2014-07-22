{-# LANGUAGE QuasiQuotes  , TemplateHaskell, NoMonomorphismRestriction,TypeFamilies, RecordWildCards  , NoImplicitPrelude
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes.Foundation  where
import Yesod
import Prelude 

import Node.Manager.Types.Acid


import Data.Acid

data NodeManager = NodeManager {
    nodes  :: (AcidState NodeManagerCellStore)
}       

mkYesodData "NodeManager" $(parseRoutesFile "node-manager-routes")


mkFoundation :: IO NodeManager
mkFoundation = do
  nmcs <- openLocalStateFrom "NodeManagerState" initNodeManagerCellStore
  return $ NodeManager {nodes = nmcs}
