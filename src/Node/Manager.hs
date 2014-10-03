{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager where


-- import Control.Applicative
-- import Data.Aeson
-- import Node.Manager.DIG

import           Node.Manager.Routes
import           Node.Manager.Routes.Foundation

-- import Prelude
import           Yesod
-- import Network.HTTP.Types.Status



nodeManagerMain :: IO ()
nodeManagerMain =
  do
  efoundation <- mkFoundation
  case efoundation of
    Left e -> print e
    Right foudnation -> warp 3000 foundation




