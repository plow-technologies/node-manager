{-# LANGUAGE QuasiQuotes  , TemplateHaskell, NoMonomorphismRestriction,TypeFamilies, RecordWildCards  
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import Node.Manager.DIG
import Node.Manager.Routes.Foundation
import Node.Manager.Types
import Prelude
import Yesod
import Data.Acid
import Network.HTTP.Types.Status
import Network.Wreq
import Network.Wreq.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

mkYesodDispatch "NodeManager" resourcesNodeManager

instance Yesod NodeManager 

-- Remember this is where your route modifiers go... Like magic   
--  maximumContentLength _ (Just (AlarmDataR )) = Just $ 2 * 1024 * 1024 * 1024
--  maximumContentLength _ _ = Just $ 2 * 1024 * 1024


-- | return all the monitored nodes
-- | / HomeR GET  

getHomeR :: Handler Value
getHomeR = do
  nodeState <- (nodes <$> getYesod)
  nodes' <- fetchStoredNodes nodeState
  return . toJSON $ nodes'


-- | insert a new nodeprocess to monitor
-- | /nodes/add AddNewR POST

postAddNewR :: Handler Value
postAddNewR = do
  nodeState <- (nodes <$> getYesod)
  rcnp <- parseJsonBody :: Handler (Result ClientNodeProc)
  case rcnp of
    Error e -> do
      sendResponseStatus status501 (toJSON e)
    Success cnp -> do
      nodes' <- insertStoredNode nodeState cnp
      liftIO $ createCheckpoint nodeState
      return . toJSON $ nodes'



-- | remove Node from monitoring
-- postDeleteNodeR :: Handler Value
-- postDeleteNodeR = do
--   nodeState <- (nodes <$> getYesod)
--   rcnp <- parseJsonBody :: Handler (


-- | get Status of a single node
getNodeR :: Handler Value
getNodeR = undefined



-- | Kill process running on a node
killNodeR :: Handler Value
killNodeR = undefined


-- | start a new node
startNodeR  :: Handler Value
startNodeR = undefined

-- | restart a node
restartNodeR :: Handler Value
restartNodeR = undefined

-- | register another manger

registerNodeR :: Handler Value
registerNodeR = undefined


-- | unregister another manager
unregisterNodeR :: Handler Value
unregisterNodeR = undefined



-- | configure nodes, add new configurations
-- | /configure/find EditConfigureR POST
postEditConfigureR :: Handler Value
postEditConfigureR = undefined

-- | /configure/add AddConfigureR POST
postAddConfigureR :: Handler Value
postAddConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of 
    Error e -> do 
      sendResponseStatus status501 (toJSON e)
    Success parsed -> do
      let mTitle = parsed ^? key "configName" . _String
      case mTitle of 
        Nothing ->  sendResponseStatus status501 (toJSON ( "Could not find field config name" :: T.Text))
        Just title -> liftIO . (LBS.writeFile $ "./configs/" ++ (T.unpack title)) . encode $ parsed

      return parsed
