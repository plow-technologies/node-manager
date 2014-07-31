{-# LANGUAGE QuasiQuotes  , TemplateHaskell, NoMonomorphismRestriction,TypeFamilies, RecordWildCards  
  , OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes where

import Control.Applicative
import Control.Lens
import Control.Exception hiding (Handler)
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.List (foldl')
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Node.Manager.DIG
import Node.Manager.Routes.Foundation
import Node.Manager.Types
import Prelude
import Yesod
import Data.Acid
import Network.HTTP.Types.Status
import Network.Wreq
import Network.Wreq.Lens
import System.IO.Error hiding (catch)
import System.Directory


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



-- | /configure/edit EditConfigureR POST
postEditConfigureR :: Handler Value
postEditConfigureR = do
  rParsed <- parseJsonBody :: Handler (Result Value)
  
  case rParsed of 
    Error e -> do sendResponseStatus status501 (toJSON e)
    Success parsed -> do 
      let ptitle = views (key "configName" . _String) T.unpack parsed
      case ptitle of 
        "" -> sendResponseStatus status501 (toJSON ( "Could not find field config name" :: T.Text))
        title -> do
          file <- liftIO $ BS.readFile $ "./configs/" ++ title ++ ".yml"
          let configure = Y.decode file :: Maybe Value
          case configure of
            Nothing -> return . toJSON $ ("" :: String)
            Just json -> do
              let editKeys = makeKeyArr parsed
                  newjson = rewriteRules json editKeys
              return newjson


makeKeyArr :: Value -> [Vedit]
makeKeyArr json = (view ( key "rewrite-rules" ._JSON )  json) 

rewriteRules :: Value -> [Vedit] -> Value
rewriteRules target edits = foldl' (\json edit -> set (members . key (editKey edit)) (editValue edit) json ) target edits


-- | /configure/add AddConfigureR POST
postAddConfigureR :: Handler Value
postAddConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of 
    Error e -> do sendResponseStatus status501 (toJSON e)
    Success parsed -> do
      let mTitle = listToMaybe $ views (_Object) (\obj -> fmap fst (HM.toList obj)) parsed
      case mTitle of 
        Nothing ->  sendResponseStatus status501 (toJSON ( "Could not find field config name" :: T.Text))
        Just title -> do
          liftIO . (LBS.writeFile $ "./configs/" ++ (T.unpack title)  ++ ".yml") . LBS.fromStrict . Y.encode $ parsed
          return parsed

-- | /configure/delete DeleteConfigureR POST
postDeleteConfigureR :: Handler Value
postDeleteConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of 
    Error e -> do sendResponseStatus status501 (toJSON e)
    Success parsed -> do 
      let pTitle = views _String T.unpack parsed
      case pTitle of 
        "" ->  sendResponseStatus status501 (toJSON ( "Cannot match blank title" :: T.Text))
        title -> do 
          liftIO . removeExisting $ ("./configs/" ++ title ++ ".yml")
          return . toJSON $ ("Success! " ++ title ++ " was removed..")
      

removeExisting :: FilePath -> IO()
removeExisting file = removeFile file `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e