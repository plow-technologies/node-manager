        {-# LANGUAGE TemplateHaskell #-}

module Node.Manager.Lens where

import           Node.Manager.Client.Types (NodeProcess (..))
import           Plow.Extras.Lens          (makeLenses_)

makeLenses_ ''NodeProcess

