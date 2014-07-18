{-# Language QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Data.Aeson
import Yesod

data Manager = Manager
data Message = Message { content :: Text }

mkYesod "Manager" [parseRoutes|
/ HomeR GET
|]

instance Yesod Manager

getHomeR :: Handler Value
getHomeR = do
	return . toJSON $ (3 :: Int)

main :: IO ()
main = warp 3000 Manager