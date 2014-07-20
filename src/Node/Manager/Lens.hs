{-# LANGUAGE  OverloadedStrings, TemplateHaskell #-}


module Node.Manager.Lens where



import Node.Manager.Client.Types


-- Local
import Node.Manager.TH



makeLenses_ ''NodeProcess

