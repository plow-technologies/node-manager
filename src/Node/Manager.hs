{-# LANGUAGE QuasiQuotes  , TemplateHaskell, NoMonomorphismRestriction,TypeFamilies, RecordWildCards  
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager where


-- import Control.Applicative
-- import Data.Aeson
-- import Node.Manager.DIG

import Node.Manager.Routes.Foundation
import Node.Manager.Routes

-- import Prelude
import Yesod
-- import Network.HTTP.Types.Status



nodeManagerMain :: IO ()
nodeManagerMain =
  do
  foundation <- mkFoundation
  warp 3000 foundation




