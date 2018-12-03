module Server where

import Reminders    as R
import Network.Wai
import Servant

type Api = R.API

api :: Proxy API
api = Proxy

server :: Server API
server = R.handler

app :: Application
app = serve api server
