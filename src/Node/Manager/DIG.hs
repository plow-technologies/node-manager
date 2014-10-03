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



insertStoredNode
  :: MonadIO m =>
     AcidState (EventState InsertNode)
     -> ClientNodeProc -> m (EventResult InsertNode)
insertStoredNode st cnp = do
  let snp = makeStorableProcess cnp
  update' st (InsertNode snp)


deleteStoredNode
  :: MonadIO m =>
     AcidState (EventState DeleteNode)
     -> Name -> m (EventResult DeleteNode)
deleteStoredNode st name = do
  update' st (DeleteNode name)


-- getStoredNode  :: MonadIO m => AcidState (EventState GetNode) -> Name -> m (EventResult GetNode)
getStoredNode
  :: MonadIO m =>
     AcidState (EventState GetNode)
     -> Name -> m (Either Text ClientNodeProc)
getStoredNode st name = do
  rslt <- query' st (GetNode name)
  maybe (return $ Left (append name "not found"))  (return . makeClientProcess) rslt


fetchStoredNodes  :: MonadIO m =>
     AcidState (EventState ReturnNodes)
     -> m (Map Text (Either Text ClientNodeProc))
fetchStoredNodes st = do
  nodes <- query' st ReturnNodes
  return $ fmap makeClientProcess  nodes


