{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Node.Manager (  initializeDirectory
                     , defaultConfigStoredPath
                    ) where


import           Control.Exception         (finally)
import           Control.Monad             (unless, void)
import           Data.Text                 (Text)
import           Filesystem                hiding (readFile, writeFile)
import           Filesystem.Path           (absolute, (</>))
import qualified Filesystem.Path.CurrentOS as OS (FilePath, fromText)
import           Network.Wai.Handler.Warp  (defaultSettings, runSettings,
                                            setHost, setPort, setTimeout)
import           Node.Client.Configs       (NodeManagerConfig (..),
                                            getHostPreference,
                                            readNodeManagerConf)
import           Node.Manager.Routes       ()
import           SimpleStore               (closeSimpleStore, createCheckpoint)
import           System.IO                 (hPrint, stderr)

import           Yesod.Core.Dispatch       (toWaiApp)

-- Default Node Manager Config Path
defaultNodeManagerConfPath :: OS.FilePath
defaultNodeManagerConfPath = OS.fromText ("node-manager-config.yml"::Text)

-- Default Config Files Stored Path
defaultConfigStoredPath :: OS.FilePath
defaultConfigStoredPath = OS.fromText ("./configs"::Text)


-- Print error to stand output
ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

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
