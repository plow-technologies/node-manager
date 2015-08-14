{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Manager.Routes where

import           Control.Exception          (catch, throwIO)
import           Control.Lens               (set, traverse, view, views, (&),
                                             (.~))
import           Control.Monad              (void)
import           Control.Monad.Trans.Either
import           Data.Aeson                 (ToJSON, Value, eitherDecode,
                                             toJSON)
import           Data.Aeson.Lens            (AsValue, key, members, _JSON,
                                             _Object, _String)
import qualified Data.ByteString            as BS (readFile)
import qualified Data.ByteString.Lazy       as LBS (fromStrict, readFile,
                                                    writeFile)
import qualified Data.HashMap.Strict        as HM (toList)
import           Data.List                  (foldl')
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import qualified Data.Yaml                  as Y
import           Filesystem                 as FS
import qualified Filesystem.Path.CurrentOS  as FPQ
import           Network.Wai                (Application)
import           Network.Wreq               hiding (Proxy)
import           Node.Manager.Client
import           Node.Manager.Types
import           Prelude                    hiding (FilePath, div, head,
                                             readFile)
import           Servant
import           System.IO                  (FilePath)
import           System.IO.Error            (isDoesNotExistError)
import           Yesod.Core                 (liftIO)

--------------------------------------------------
--------------------------------------------------

-- | Configuration API


-- | make a list of rewrite rules
-- fromJSON
makeKeyArr :: Value -> [Vedit]
makeKeyArr = view ( key "rewrite-rules" ._JSON )

-- | Take a Value and run through it one level, replacing anyting found with the incoming rewrite
-- rule

rewriteRules :: Value -> [Vedit] -> Value
rewriteRules  = foldl' (\j edit -> j & (members . key (editKey edit)) .~ (editValue edit) )

writeConfigFile :: (AsValue s, ToJSON s) => s -> IO ()
writeConfigFile parsed' = do
  let mTitle = listToMaybe $ views _Object (\obj -> fmap fst . HM.toList $  obj) parsed'
  case mTitle of
        Nothing -> putStrLn "Could not find field config name"
        Just t -> liftIO . LBS.writeFile ("./configs/" ++ T.unpack t  ++ ".yml") . LBS.fromStrict . Y.encode $ parsed'

removeExisting :: FPQ.FilePath -> IO()
removeExisting file = removeFile file `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


-------------------------------- Servant ---------------------------------


server :: Server API
server = retrieveConfig
    :<|> addConfig
    :<|> deleteConfig
    :<|> copyConfig
    :<|> cloneDir
    :<|> getConfigure
    :<|> editConfig
    :<|> docs
  where
    retrieveConfig :: Value -> EitherT ServantErr IO Value
    retrieveConfig configuration  = do
      let mTitle = views (key "configName" . _String) T.unpack configuration
      case mTitle of
        "" -> do
          liftIO $ print "Could not find field config name"
          return $ toJSON ( "Could not find field config name" :: T.Text)
        t -> do
          liftIO $ print "running retrieveConfig"
          file <- liftIO $ BS.readFile $ "./configs/" ++ t ++ ".yml"
          case (Y.decode file :: Maybe Value) of
            Nothing -> return . toJSON $ ("" :: String)
            Just json -> do
              let editKeys = makeKeyArr configuration
              return $ rewriteRules json editKeys
    addConfig :: Value -> EitherT ServantErr IO Value
    addConfig config = do
      let mTitle = listToMaybe $ views _Object (\obj -> fmap fst . HM.toList $  obj) config
      case mTitle of
        Nothing -> do
          liftIO $ print "Could not find field config name"
          return $ toJSON ( "Could not find field config name" :: T.Text)
        Just t -> do
          liftIO $ print "running addConfig"
          liftIO . LBS.writeFile ("./configs/" ++ T.unpack t  ++ ".yml") . LBS.fromStrict . Y.encode $ config
          return config
    editConfig :: Value -> EitherT ServantErr IO Value
    editConfig config = do
      let mTitle = listToMaybe $ views _Object (\obj -> fmap fst . HM.toList $  obj) config
      case mTitle of
        Nothing -> do
          liftIO $ print "Could not find field config name"
          return $ toJSON ( "Could not find field config name" :: T.Text)
        Just t -> do
          liftIO $ print "running editConfig"
          bsSettings <- liftIO $ BS.readFile $ "./configs/" ++ T.unpack t  ++ ".yml"
          case (Y.decode bsSettings :: Maybe Value) of
            Nothing -> do
              liftIO $ print "Invalid Value"
              return ("" :: Value)
            Just json -> do
              let editKeys = makeKeyArr config
              let settings = rewriteRules json editKeys
              addConfig settings
    deleteConfig :: Value -> EitherT ServantErr IO Value
    deleteConfig config = do
      let pTitle = views _String T.unpack config
      case pTitle of
        "" ->  return $ toJSON ( "Cannot match blank title" :: T.Text)
        title' -> do
           liftIO $ print "running deleteConfig"
           liftIO . removeExisting . FPQ.fromText . T.pack $ ("./configs/" ++ title' ++ ".yml")
           return . toJSON $ ("Success! " ++ title' ++ " was removed..")
    copyConfig :: Value -> EitherT ServantErr IO Value
    copyConfig config = do
      let pTarget = views (key "route" . _String) T.unpack config
      case pTarget of
        "" -> return $ toJSON ( "Cannot copy to an empty URL" :: T.Text)
        target -> do
          directoryExist <- liftIO $ isDirectory "./configs"
          case directoryExist of
            False -> return $ toJSON ("/configs directory does not exist" :: T.Text)
            True -> do
              allConfigPaths <- liftIO $ listDirectory "./configs"
              fileList <- liftIO $ traverse readFile allConfigPaths
              let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
              void $ liftIO $ traverse (post target) jsonList
              return . toJSON $ ("Copy Success" :: String)
    cloneDir :: Value -> EitherT ServantErr IO Value
    cloneDir config = do
      let pTarget = views (key "directoryName" . _String) T.unpack config
      case pTarget of
        "" -> return $ toJSON ( "Cannot copy an empty diretory" :: T.Text)
        target -> do
          let targetDir = FPQ.fromText . T.pack $ target
          directoryExist <- liftIO $ isDirectory . FPQ.fromText . T.pack $ target
          case directoryExist of
            False -> return $ toJSON ( T.pack (target ++  "  does not exist"))
            True -> do
              allConfigPaths <- liftIO $ listDirectory targetDir
              fileList <- liftIO $ traverse readFile allConfigPaths
              let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
              void $ liftIO $ traverse writeConfigFile jsonList
              return $ toJSON ("Copy Success" :: String)
    getConfigure :: EitherT ServantErr IO Value
    getConfigure = do
      directoryExist <- liftIO $ isDirectory "./configs"
      case directoryExist of
        False -> return $ toJSON ( "/configs directory does not exist" :: T.Text)
        True -> do
          allConfigPaths <- liftIO $ listDirectory "./configs"
          fileList <- liftIO $ traverse readFile allConfigPaths
          let jsonList = catMaybes (map Y.decode fileList :: [Maybe Value])
          return . toJSON $ jsonList
    docs = serveDirectory ("src/Node/Manager/Static" :: FilePath)

app :: Application
app = serve userAPI server
