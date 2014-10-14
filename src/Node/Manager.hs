{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager (  buildNodeManager
                     , startNodeManager
                     , startServer
                    ) where

import           Filesystem
import qualified Filesystem.Path                as FP
import qualified Filesystem.Path.CurrentOS      as OS
import           Network.Wai.Handler.Warp
import           Node.Manager.Config
import           Node.Manager.Routes            ()
import           Node.Manager.Routes.Foundation
import           SimpleStore
import           System.IO                      (hPrint, stderr)
import           System.Posix.Resource
import           Yesod

defaultNodeManagerConfPath :: FilePath
defaultNodeManagerConfPath = "nodeManagerConfig.yml"

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
  nmcs <- initializeSimpleStore . nodeManagerFilePath $ nc
  print ("Initializing store done"::Text)
  return
     NodeManager {nodes=nmcs}

startNodeManager :: IO ()
startNodeManager = do
  nc <- readAlarmConf defaultNodeManagerConfPath
  nmFoundation <- buildNodeManager nc
  finally ( do
             print ("Starting"::Text)
             void $ forkFinally (startServer nc nmFoundation) (\e -> ePrint "Node Manager crash" >> ePrint e)
             ) (void $ do
                let msg :: Text
                    msg = "Closing Node Manager Server"
                hPrint stderr msg
                createCheckPoint (nodes nmFoundation))

startServer :: NodeManagerConfig -> NodeManager -> IO ()
startServer nc nmFoundation =  do
  app <- toWaiApp nmFoundation
  let nodeManagerDefaults = setTimeout (3*60).
                            (setHost.getHostPreference.nodeManagerHost $ nc).
                              (setPort.nodeManagerPort $ nc) $ defaultSettings
  runSettings nodeManagerDefaults app





