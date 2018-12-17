{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Concurrent as C
import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.Either
import           Data.Time.Calendar as T
import           Data.Time.Clock as T
import qualified Servant.QuickCheck as SQ
import           Test.Hspec
import qualified Test.QuickCheck as TQ

import qualified Client as Client
import           Prelude hiding (repeat)
import           Reminders
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
  servantQuickcheckSpec

-- | Tests for business logic using @hspec@
businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withRemindersApp $ do

    let url = "http://localhost:8080"

    -- testing scenarios
    describe "POST /reminder" $ do
      it "should fail to create a reminder with a Yearly repeat" $ do
        res <- Client.createReminder url r
        res `shouldSatisfy` isLeft

      it "should create a reminder" $ do
        res <- Client.createReminder url rr
        res `shouldSatisfy` isRight

  where
    r = Reminder
        { title    = "Christmas Day"
        , onADay   = T.UTCTime (T.fromGregorian 2019 12 25) (T.secondsToDiffTime 540)
        , repeat   = Yearly
        , priority = High
        , note     = Nothing
        }
    rr = r { repeat = NoRepeat }

-- | Properties that ought to hold true of the entire API
servantQuickcheckSpec :: Spec
servantQuickcheckSpec = describe "reminders server" $ do
  it "follows best practices" $
    SQ.withServantServer Server.api (pure Server.server) $ \burl ->
      SQ.serverSatisfies Server.api burl TQ.stdArgs
        (SQ.not500 SQ.<%>
         SQ.onlyJsonObjects SQ.<%>
         mempty)


-- | Uses hspec to run all specs
main :: IO ()
main = hspec spec
