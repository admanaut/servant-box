{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- import           Control.Lens ((&), (.~), (?~))
import           Data.Proxy
import qualified Data.Swagger as SW
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

spec :: SW.Swagger
spec = toSwagger (Proxy :: Proxy Api)

main :: IO ()
main = print spec
