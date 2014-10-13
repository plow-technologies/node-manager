{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager where

import           Node.Manager.Routes            ()
import           Node.Manager.Routes.Foundation
import           Yesod

nodeManagerMain :: IO ()
nodeManagerMain =
  do
  foundation <- mkFoundation
  warp 3000 foundation




