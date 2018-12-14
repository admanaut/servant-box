{-# LANGUAGE TypeOperators   #-}

module Server where

import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Reminders
import           Servant
import           Swagger

type API = RemindersAPI :<|> SwaggerAPI

api :: Proxy API
api = Proxy

server :: Server API
server = reminders :<|> swagger

app :: Application
app = serve api server

run :: Int -> IO ()
run port = Warp.run port app
