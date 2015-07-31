{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Node.Manager.Client where

import           Control.Lens               hiding ((.=))
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Lens
import           Servant
import           Servant.Client


type API = "configure" :> "edit" :> ReqBody '[JSON] Value :> Post '[JSON] Value
      :<|> "configure" :> "add" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "delete" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "copy" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "clone" :> ReqBody '[JSON] Value :>  Post '[JSON] Value
      :<|> "configure" :> "get" :> Get '[JSON] Value
      :<|> Raw

userAPI :: Proxy API
userAPI = Proxy

editConfig :: Value -> EitherT ServantError IO Value
addConfig :: Value -> EitherT ServantError IO Value
deleteConfig :: Value -> EitherT ServantError IO Value
copyConfig :: Value -> EitherT ServantError IO Value
cloneDirectory :: Value -> EitherT ServantError IO Value
getConfig :: EitherT ServantError IO Value
-- docs :: EitherT ServantError IO Raw
editConfig :<|> addConfig :<|> deleteConfig :<|> copyConfig :<|> cloneDirectory :<|> getConfig :<|> docs = client userAPI (BaseUrl Http "localhost" 8080)
