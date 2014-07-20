{-# LANGUAGE  OverloadedStrings, TemplateHaskell #-}


module Node.Manager.TH where

import Control.Lens

-- | underscore appended instead of prepended
-- | someRecordAccess -> someRecordAccess_ 

makeLenses_ t = makeLensesWith ?? t $ defaultRules & lensField .~ \x -> Just (x ++ "_")
