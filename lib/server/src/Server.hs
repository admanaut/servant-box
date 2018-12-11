{-# LANGUAGE TypeOperators   #-}

module Server where

import Reminders
import Swagger
import Network.Wai
import Servant

type API = RemindersAPI :<|> SwaggerAPI

api :: Proxy API
api = Proxy

server :: Server API
server = reminders :<|> swagger

app :: Application
app = serve api server
