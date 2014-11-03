{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager (  buildNodeManager
                     , startNodeManager
                     , startServer
                     , initializeDirectory
                     , defaultConfigStoredPath
                    ) where


import           Control.Exception              (finally)
import           Control.Monad                  (unless, void)
import           Data.Text                      (Text)
import           Filesystem                     hiding (readFile, writeFile)
import           Filesystem.Path                (absolute, (</>))
import qualified Filesystem.Path.CurrentOS      as OS (FilePath, fromText)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setHost, setPort, setTimeout)
import           Node.Client.Configs            (NodeManagerConfig (..),
                                                 getHostPreference,
                                                 readNodeManagerConf)
import           Node.Manager.Routes            ()
import           Node.Manager.Routes.Foundation (NodeManager (..))
import           Node.Manager.Types.SimpleStore (initializeSimpleStore)
import           SimpleStore                    (closeSimpleStore,
                                                 createCheckpoint)
import           System.IO                      (hPrint, stderr)
import           System.Posix.Resource          (Resource (ResourceOpenFiles),
                                                 ResourceLimit (ResourceLimit),
                                                 getResourceLimit, hardLimit,
                                                 softLimit)
import           Yesod.Core.Dispatch            (toWaiApp)

-- Default Node Manager Config Path
defaultNodeManagerConfPath :: OS.FilePath
defaultNodeManagerConfPath = OS.fromText ("node-manager-config.yml"::Text)

-- Default Config Files Stored Path
defaultConfigStoredPath :: OS.FilePath
defaultConfigStoredPath = OS.fromText ("./configs"::Text)

-- Get Files Limit
getNumFilesLimit :: IO (Integer, Integer)
getNumFilesLimit = do
  lm <- getResourceLimit ResourceOpenFiles
  let (ResourceLimit sl) = softLimit lm
      (ResourceLimit hl) = hardLimit lm
  return (sl,hl)

-- Print error to stand output
ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

-- Build Node Manger Yesod Fundation
buildNodeManager :: NodeManagerConfig ->  IO NodeManager
buildNodeManager nc  =  do
  getNumFilesLimit >>= print
  putStrLn "NodeManager Initializing ..."
  putStrLn  "Initializing store"
  nmcs <- initializeSimpleStore . OS.fromText .  managerFilePath $ nc
  putStrLn "Initializing store done"
  return NodeManager {nodes=nmcs}

-- Make Absolute File Path
makeAbsoluteFp :: OS.FilePath -> IO OS.FilePath
makeAbsoluteFp fp =
  if absolute fp
    then return fp
    else do
      base <- getWorkingDirectory
      return $ base </> fp

-- Initialize File Directory
initializeDirectory :: OS.FilePath -> IO ()
initializeDirectory dir = do
  putStrLn "Initializing config files stored directory"
  fp <- makeAbsoluteFp dir
  exists <- isDirectory fp
  unless exists $ do
        createDirectory True fp
        putStrLn "Sucessfully Created Configs stored Diretory."

-- Start Node Manager Server
startNodeManager :: IO ()
startNodeManager = do
  nc <- readNodeManagerConf defaultNodeManagerConfPath
  nmFoundation <- buildNodeManager nc
  initializeDirectory defaultConfigStoredPath
  finally (
         putStrLn "Starting ..." >> startServer nc nmFoundation
           ) (void $ do
                 let msg :: Text
                     msg = "Closing Node Manager Server"
                 ePrint msg
                 void $  createCheckpoint (nodes nmFoundation)
                 closeSimpleStore (nodes nmFoundation))

-- Start Warp Server
startServer :: NodeManagerConfig -> NodeManager -> IO ()
startServer nc nmFoundation =  do
  app <- toWaiApp nmFoundation
  let nodeManagerDefaults = setTimeout (3*60).
                            (setHost.getHostPreference.nodeManagerHost $ nc).
                              (setPort.nodeManagerPort $ nc) $ defaultSettings
  runSettings nodeManagerDefaults app





