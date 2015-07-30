{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.IO.Class   (liftIO)
import           Network.Wai.Handler.Warp (run)
import           Node.Manager.Routes

main :: IO ()
main = do
  liftIO $ print "running at localhost:8080"
  run 8080 app
