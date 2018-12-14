{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Concurrent as C
import           Data.Either
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Test.Hspec

import qualified Client as Client
import qualified Server as Server

-- | Spin up a server in a separate thread
withRemindersApp :: IO () -> IO ()
withRemindersApp action =
  bracket (liftIO $ C.forkIO $ Server.run 8080)
    C.killThread
    (const action)

spec :: Spec
spec = do
  businessLogicSpec

businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withRemindersApp $ do

    let url = "http://localhost:8080"

    -- testing scenarios
    describe "GET /reminders" $ do
      it "should get a list of reminders" $ do
        reminders <- Client.listReminders url
        reminders `shouldSatisfy` isRight

-- | Uses hspec to run all specs
main :: IO ()
main = hspec spec
