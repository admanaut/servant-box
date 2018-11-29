{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Server where

import Data.Aeson.TH
import Data.Aeson
import Network.Wai
import Servant

data Pong = Pong

server :: Server API
server = return Pong

type API
  = "ping" :> Get '[JSON] Pong

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

deriveJSON defaultOptions ''Pong
