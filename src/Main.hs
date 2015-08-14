{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Monad.IO.Class   (liftIO)
import           Network.Wai.Handler.Warp (run)
import           Node.Client.Configs
import           Node.Manager.Routes

main :: IO ()
main = do
  conf <- readNodeManagerConf "configs/node-manager-config.yml"
  liftIO $ print ("running at " ++ show (nodeManagerPort conf))
  run (nodeManagerPort conf) app
