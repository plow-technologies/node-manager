{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Node.Manager.TH where

import           Control.Lens
import           Data.Char           (toLower)
import           Language.Haskell.TH

-- | underscore appended instead of prepended
-- | someRecordAccess -> someRecordAccess_

-- makeLenses_ t = makeLensesWith ?? t $ defaultRules & lensField .~ \x -> Just (x ++ "_")

makeLenses_ t = makeLensesWith ?? t $ lensRules & lensField .~ lFcn
 where
   lFcn _ n = case nameBase n of
                x:xs -> [TopName (mkName ((toLower x:xs) ++ "_"))]
                [] -> []
