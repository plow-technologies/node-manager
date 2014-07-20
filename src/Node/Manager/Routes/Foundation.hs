{-# LANGUAGE QuasiQuotes  , TemplateHaskell, NoMonomorphismRestriction,TypeFamilies, RecordWildCards  , NoImplicitPrelude
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes.Foundation  where
import Yesod

import Node.Manager.Types.Acid

data NodeManager = NodeManager {
    nodes  :: NodeManagerCellStore 
}       

mkYesodData "NodeManager" $(parseRoutesFile "node-manager-routes")



