{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes where

import           Control.Exception              (catch, throwIO)
import           Control.Lens                   (set, traverse, view, views)
import           Control.Monad                  (void)
import           Data.Aeson                     (Result (..), ToJSON, Value,
                                                 toJSON)
import           Data.Aeson.Lens                (AsValue, key, members, _JSON,
                                                 _Object, _String)
import qualified Data.ByteString                as BS (readFile)
import qualified Data.ByteString.Lazy           as LBS (fromStrict, writeFile)
import qualified Data.HashMap.Strict            as HM (toList)
import           Data.List                      (foldl')
import           Data.Maybe                     (catMaybes, listToMaybe)
import qualified Data.Text                      as T
import qualified Data.Yaml                      as Y
import           Filesystem                     as FS
import           Filesystem.Path.CurrentOS
import           Network.HTTP.Types.Status
import           Network.Wreq
import           Node.Manager.DIG
import           Node.Manager.Routes.Foundation
import           Node.Manager.Types
import           Prelude                        hiding (FilePath, div, head,
                                                 readFile)
import           SimpleStore                    (createCheckpoint)
import           System.IO.Error                (isDoesNotExistError)
import           Yesod.Core                     (Yesod, getYesod, liftIO,
                                                 mkYesodDispatch, parseJsonBody,
                                                 sendResponseStatus)

import           Text.Blaze.Html5               (Html, body, head, p, title)
-- import qualified Text.Blaze.Html5.Attributes    as A

mkYesodDispatch "NodeManager" resourcesNodeManager

instance Yesod NodeManager

-- Remember this is where your route modifiers go... Like magic
--  maximumContentLength _ (Just (AlarmDataR )) = Just $ 2 * 1024 * 1024 * 1024
--  maximumContentLength _ _ = Just $ 2 * 1024 * 1024

documentation :: Html
documentation = do
    head $ do
        title "Node Manager"
    body $ do
        p $ "Node Manager Documentation"
        p $ "Site Navigation: "
        p $ "/configure/get            -- Show all the Config Files              -- Eg:  post 'http://some.lame.nodemanager.com/configure/add' (toJSON (object ['alarm-state-config' .= object  [ ( 'tag' .= 2), ('src' .= (object ['almKeySrc' .= (object [ 'unSText' .=  'onping.plowtech.net'])])),  ('host' .= 'www.stupidurl.com'), ('port'.= 2)]]))"
        p $ "/configure/edit           -- Edit the Config File                   -- Eg: post 'http://some.lame.nodemanager.com/configure/add' (toJSON (object ['alarm-state-config' .= object  [ ( 'tag' .= 2000), ('src' .= (object ['almKeySrc' .= (object [ 'unSText' .=  'onping.plowtech.net'])])),  ('host' .= 'www.stupidurl.com'), ('port'.= 2000)]]))"
        p $ "/configure/delete                                                   -- Delete the Config File -- Eg: post 'http://some.lame.nodemanager.com/configure/edit' (toJSON $ object ['configName' .= 'alarm-state-config', 'rewrite-rules' .= (object [('key' .= 'port') , ('val' .= 2)])])"
        p $ "/configure/copy                                                     -- Copy the Config File -- Eg: post 'http://some.lame.nodemanager.com/configure/delete' (toJSON 'alarm-state-config')"
        p $ "Production Node Manager        -- 108.168.240.123:2533"
        p $ "Staging Node Manager           -- 54.69.197.241:2733"
        p $ "LocalHost Node Manager         -- 54.69.197.241:2833"
        p $ "Note: If you want to change a configure file, do not do it on your local path. User rewrite route '/configure/edit' or change it on the server under the path /configs."


-- | return all the monitored nodes
-- | / HomeR GET

getHomeR :: Handler Html
getHomeR = do
  (NodeManager{nodes=nodeState}) <- getYesod
  nodes' <- liftIO $ fetchStoredNodes nodeState
  return documentation
  -- return . toJSON $ nodes'


-- | insert a new nodeprocess to monitor
-- | /nodes/add AddNewR POST

postAddNewR :: Handler Value
postAddNewR = do
  (NodeManager{nodes=nodeState}) <- getYesod
  rcnp <- parseJsonBody :: Handler (Result ClientNodeProc)
  case rcnp of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success cnp -> do
      nodes' <- liftIO $ insertStoredNode nodeState cnp
      void $ liftIO $ createCheckpoint nodeState
      return . toJSON $ nodes'

-- API for Node Management
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

-- Node Management API End
makeKeyArr :: Value -> [Vedit]
makeKeyArr = view ( key "rewrite-rules" ._JSON )

rewriteRules :: Value -> [Vedit] -> Value
rewriteRules  = foldl' (\j edit -> set (members . key (editKey edit)) (editValue edit) j)

writeConfigFile :: (AsValue s, ToJSON s) => s -> IO ()
writeConfigFile parsed' = do
  let mTitle = listToMaybe $ views _Object (\obj -> fmap fst . HM.toList $  obj) parsed'
  case mTitle of
        Nothing -> putStrLn "Could not find field config name"
        Just title -> liftIO . LBS.writeFile ("./configs/" ++ T.unpack title  ++ ".yml") . LBS.fromStrict . Y.encode $ parsed'

removeExisting :: FilePath -> IO()
removeExisting file = removeFile file `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | /configure/edit EditConfigureR POST
postEditConfigureR :: Handler Value
postEditConfigureR = do
  rParsed <- parseJsonBody :: Handler (Result Value)
  case rParsed of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success parsed -> do
      case views (key "configName" . _String) T.unpack parsed of
        "" -> sendResponseStatus status501 (toJSON ( "Could not find field config name" :: T.Text))
        title -> do
          file <- liftIO $ BS.readFile $ "./configs/" ++ title ++ ".yml"
          case (Y.decode file :: Maybe Value) of
            Nothing -> return . toJSON $ ("" :: String)
            Just json -> do
              let editKeys = makeKeyArr parsed
              return $ rewriteRules json editKeys


-- | /configure/add AddConfigureR POST
postAddConfigureR :: Handler Value
postAddConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success parsed' -> do
      let mTitle = listToMaybe $ views _Object (\obj -> fmap fst . HM.toList $  obj) parsed'
      case mTitle of
        Nothing ->  sendResponseStatus status501 (toJSON ( "Could not find field config name" :: T.Text))
        Just title -> do
          liftIO . LBS.writeFile ("./configs/" ++ T.unpack title  ++ ".yml") . LBS.fromStrict . Y.encode $ parsed'
          return parsed'

-- | /configure/delete DeleteConfigureR POST
postDeleteConfigureR :: Handler Value
postDeleteConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success parsed' -> do
      let pTitle = views _String T.unpack parsed'
      case pTitle of
        "" ->  sendResponseStatus status501 (toJSON ( "Cannot match blank title" :: T.Text))
        title -> do
           liftIO . removeExisting . fromText . T.pack $ ("./configs/" ++ title ++ ".yml")
           return . toJSON $ ("Success! " ++ title ++ " was removed..")

-- | /configure/copy CopyConfigureR POST
postCopyConfigureR :: Handler Value
postCopyConfigureR = do
  parsed <- parseJsonBody :: Handler (Result Value)
  case parsed of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success parsed' -> do
      let pTarget = views (key "route" . _String) T.unpack parsed'
      case pTarget of
        "" -> sendResponseStatus status501 (toJSON ( "Cannot copy to an empty URL" :: T.Text))
        target -> do
          directoryExist <- liftIO $ isDirectory "./configs"
          case directoryExist of
            False -> sendResponseStatus status501 (toJSON ( "/configs directory does not exist" :: T.Text))
            True -> do
              allConfigPaths <- liftIO $ listDirectory "./configs"
              fileList <- liftIO $ traverse readFile allConfigPaths
              let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
              void $ liftIO $ traverse (post target) jsonList
              return . toJSON $ ("Copy Success" :: String)

-- | /configure/clone/diretory CloneDiretoryR POST
postCloneDiretoryR :: Handler Value
postCloneDiretoryR = do
   parsed <- parseJsonBody :: Handler (Result Value)
   case parsed of
    Error e -> sendResponseStatus status501 (toJSON e)
    Success parsed' -> do
      let pTarget = views (key "directoryName" . _String) T.unpack parsed'
      case pTarget of
        "" -> sendResponseStatus status501 (toJSON ( "Cannot copy an empty diretory" :: T.Text))
        target -> do
          let targetDir = fromText . T.pack $ target
          directoryExist <- liftIO $ isDirectory . fromText . T.pack $ target
          case directoryExist of
            False -> sendResponseStatus status501 (toJSON ( T.pack (target ++  "  does not exist")))
            True -> do
              allConfigPaths <- liftIO $ listDirectory targetDir
              fileList <- liftIO $ traverse readFile allConfigPaths
              let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
              void $ liftIO $ traverse writeConfigFile jsonList
              return . toJSON $ ("Copy Success" :: String)

getConfigureR :: Handler Value
getConfigureR = do
  directoryExist <- liftIO $ isDirectory "./configs"
  case directoryExist of
            False -> sendResponseStatus status501 (toJSON ( "/configs directory does not exist" :: T.Text))
            True -> do
              allConfigPaths <- liftIO $ listDirectory "./configs"
              fileList <- liftIO $ traverse readFile allConfigPaths
              let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
              return . toJSON $ jsonList


-- buildFilePaths :: [String] -> IO [FilePath]
-- buildFilePaths titles = do
--       let paths =  fromText . T.pack <$> titles
--       return paths

-- postCloneConfigureR :: Handler Value
-- postCloneConfigureR = do
--   parsed <- parseJsonBody :: Handler (Result Value)
--   case parsed of
--     Error e -> sendResponseStatus status501 (toJSON e)
--     Success parsed -> do
--       let pTarget = views (key "route" . _String) T.unpack parsed
--       case pTarget of
--         "" -> sendResponseStatus status501 (toJSON ( "Cannot copy to an empty URL" :: T.Text))
--         target -> do
--           directoryExist <- liftIO $ isDirectory "./configs"
--           case directoryExist of
--             False -> sendResponseStatus status501 (toJSON ( "/configs directory does not exist" :: T.Text))
--             True -> do
--               let cloneList = views (key (T.unpack "nameList") . values)  parsed
--               allConfigPaths <- liftIO $ listDirectory "./configs"
--               fileList <- liftIO $ traverse readFile allConfigPaths
--               let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
--               liftIO $ traverse (post target) jsonList
--               return . toJSON $ ("" :: String)
