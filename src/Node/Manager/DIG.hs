{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Node.Manager.DIG (
                          insertStoredNode
                        , deleteStoredNode
                        , getStoredNode
                        , fetchStoredNodes
                        ) where

-- General
import           Control.Lens
import           Control.Monad        (void)
import           Node.Manager.Lens
import           Node.Manager.Types
import           Prelude
import           SimpleStore          (SimpleStore)
-- Serialization
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Text
-- Containers
import           Data.Map.Strict


-- | There is a natural asymmetry between any parser and encoder ...
-- | so makeStorableProc, which serializes gets no Maybe on the return,
-- | while make Client Process gets a Result

makeStorableProcess :: ClientNodeProc -> StorableNodeProc
makeStorableProcess = over checkBody_ (BL.toStrict.encode)

makeClientProcess :: StorableNodeProc -> Either Text ClientNodeProc
makeClientProcess txtNodeproc = case views checkBody_ (eitherDecode' . BL.fromStrict) txtNodeproc of
                                 (Left s) -> Left . pack $ s
                                 (Right v)  -> Right $ set checkBody_ v txtNodeproc

insertStoredNode :: SimpleStore NodeManagerCellStore -> ClientNodeProc -> IO ()
insertStoredNode st cnp = do
     let snp = makeStorableProcess cnp
     void $ insertNode st snp

deleteStoredNode :: SimpleStore NodeManagerCellStore -> Text -> IO ()
deleteStoredNode st name = void $ deleteNode st name

getStoredNode :: SimpleStore (Map Text StorableNodeProc) -> Text -> IO (Either Text ClientNodeProc)
getStoredNode st name = do
      rslt <- getNode st name
      maybe (return $ Left (append name "not found")) (return . makeClientProcess) rslt

fetchStoredNodes :: SimpleStore NodeManagerCellStore -> IO (Map Text (Either Text ClientNodeProc))
fetchStoredNodes st = do
      nodes' <- returnNodes st
      return $ fmap makeClientProcess (getNodes nodes')

