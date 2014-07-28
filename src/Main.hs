{-# Language QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

import Data.Aeson
import Yesod
import Data.Text


import Node.Manager

main :: IO ()
main = nodeManagerMain
