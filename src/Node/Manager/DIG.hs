{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module Node.Manager.DIG (
                          insertStoredNode
                        , deleteStoredNode
                        , getStoredNode
                        , fetchStoredNodes
                        ) where

import           Control.Lens
import           Node.Manager.Lens
import           Node.Manager.Types
import           Prelude
-- import Control.Applicative
import           Control.Monad          (void)
import           Control.Monad.IO.Class


-- Serialization
import           Data.Aeson
import           Data.Text
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy   as BL

-- Containers
import           Data.Map.Strict


-- | There is a natural asymmetry between any parser and encoder ...
-- | so makeStorableProc, which serializes gets no Maybe on the return,
-- | while make Client Process gets a Result

makeStorableProcess :: ClientNodeProc -> StorableNodeProc
makeStorableProcess txtNodeproc = over checkBody_ (BL.toStrict.encode) txtNodeproc

makeClientProcess :: StorableNodeProc -> Either Text ClientNodeProc
makeClientProcess txtNodeproc = case views checkBody_ (eitherDecode' . BL.fromStrict) txtNodeproc of
                                 (Left s) -> Left . pack $ s
                                 (Right v)  -> Right $ set checkBody_ v txtNodeproc

insertStoredNode st cnp = do
     let snp = makeStorableProcess cnp
     void $ insertNode st snp

deleteStoredNode st name = do
     void $ deleteNode st name

getStoredNode st name = do
      rslt <- getNode st name
      maybe (return $ Left (append name "not found")) (return . makeClientProcess) rslt

fetchStoredNodes st = do
      nodes <- returnNodes st
      return $ fmap makeClientProcess nodes

