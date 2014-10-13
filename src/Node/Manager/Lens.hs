{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Node.Manager.Lens where

import           Node.Manager.Client.Types
import           Node.Manager.TH

makeLenses_ ''NodeProcess

