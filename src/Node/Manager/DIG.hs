{-# LANGUAGE QuasiQuotes, RecordWildCards, NoImplicitPrelude
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Node.Manager.DIG () where


import Node.Manager.Lens
import Node.Manager.Types
import Prelude 
import Control.Lens
-- import Control.Applicative
-- import Control.Monad


-- Serialization
import Data.Text
import Data.Aeson
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
-- Acid

-- import Data.Aci

-- Containers
-- import Data.Map.Strict
-- import Data.ByteString

-- Local
-- import Node.Manager.Client



-- | There is a natural asymmetry between any parser and encoder ...
-- | so makeStorableProc, which serializes gets no Maybe on the return,
-- | while make Client Process gets a Result

makeStorableProcess :: ClientNodeProc -> StorableNodeProc
makeStorableProcess txtNodeproc = over checkBody_ (BL.toStrict.encode) txtNodeproc



makeClientProcess :: StorableNodeProc -> Either Text ClientNodeProc  
makeClientProcess txtNodeproc = case views checkBody_ (eitherDecode' . BL.fromStrict) txtNodeproc of
                                 (Left s) -> Left . pack $ s
                                 (Right v)  -> Right $ set checkBody_ v txtNodeproc 




