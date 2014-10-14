{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
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
import           Filesystem.Path
import qualified Filesystem.Path.CurrentOS      as OS
import           Network.Wai.Handler.Warp
import           Node.Manager.Config
import           Node.Manager.Routes            ()
import           Node.Manager.Routes.Foundation
import           Node.Manager.Types.SimpleStore (initializeSimpleStore)
import           SimpleStore                    (closeSimpleStore,
                                                 createCheckpoint)
import           System.IO                      (hPrint, stderr)
import           System.Posix.Resource
import           Yesod                          (toWaiApp)

defaultNodeManagerConfPath :: OS.FilePath
defaultNodeManagerConfPath = OS.fromText ("node-manager-config.yml"::Text)

defaultConfigStoredPath :: OS.FilePath
defaultConfigStoredPath = OS.fromText ("./configs"::Text)

getNumFilesLimit :: IO (Integer, Integer)
getNumFilesLimit = do
  lm <- getResourceLimit ResourceOpenFiles
  let (ResourceLimit sl) = softLimit lm
      (ResourceLimit hl) = hardLimit lm
  return (sl,hl)

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

buildNodeManager :: NodeManagerConfig ->  IO NodeManager
buildNodeManager nc  =  do
  getNumFilesLimit >>= print
  print ("NodeManager Initializing ..." :: Text)
  print ("Initializing store"::Text)
  nmcs <- initializeSimpleStore . OS.fromText .  managerFilePath $ nc
  print ("Initializing store done"::Text)
  return NodeManager {nodes=nmcs}

makeAbsoluteFp :: OS.FilePath -> IO OS.FilePath
makeAbsoluteFp fp =
  if absolute fp
    then return fp
    else do
      base <- getWorkingDirectory
      return $ base </> fp

initializeDirectory :: OS.FilePath -> IO ()
initializeDirectory dir = do
  print ("Initializing config files stored directory"::Text)
  fp <- makeAbsoluteFp dir
  exists <- isDirectory fp
  unless exists $ do
        createDirectory True fp
        print ("Sucessfully Created Configs stored Diretory."::Text)

startNodeManager :: IO ()
startNodeManager = do
  nc <- readNodeManagerConf defaultNodeManagerConfPath
  nmFoundation <- buildNodeManager nc
  initializeDirectory defaultConfigStoredPath
  finally (
         print ("Starting ..."::Text) >> startServer nc nmFoundation
           ) (void $ do
                 let msg :: Text
                     msg = "Closing Node Manager Server"
                 ePrint msg
                 void $  createCheckpoint (nodes nmFoundation)
                 closeSimpleStore (nodes nmFoundation))


startServer :: NodeManagerConfig -> NodeManager -> IO ()
startServer nc nmFoundation =  do
  app <- toWaiApp nmFoundation
  let nodeManagerDefaults = setTimeout (3*60).
                            (setHost.getHostPreference.nodeManagerHost $ nc).
                              (setPort.nodeManagerPort $ nc) $ defaultSettings
  runSettings nodeManagerDefaults app





