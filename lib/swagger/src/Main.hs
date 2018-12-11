{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Lens ((&), (.~), (?~))
import           Data.Aeson
import           Data.Proxy
import qualified Data.Swagger as SW
import qualified Data.Text as Tx
import           Reminders
import           Servant.Swagger
import           Server

-- | To be able to generate Swagger schema,
-- all datatypes have to have an instance
-- of ToSchema
instance SW.ToSchema Priority
instance SW.ToSchema Repeat
instance SW.ToSchema Reminder
instance SW.ToSchema a => SW.ToSchema (WithId a)

-- | Generates the Swagger spec and adds more meta
spec :: SW.Swagger
spec = toSwagger (Proxy :: Proxy Api)
  & (SW.info . SW.title) .~ Tx.pack "Reminders API"
  & (SW.info . SW.version) .~ Tx.pack "1.0"
  & (SW.info . SW.description) ?~ Tx.pack "This is an API for the Reminders service"
  & SW.host ?~ SW.Host "localhost" (Just 8080)


-- | execute this to generate JSON spec
main :: IO ()
main = print (encode spec)
