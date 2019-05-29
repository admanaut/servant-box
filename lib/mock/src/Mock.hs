{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mock where

import Reminders
import Servant
import Servant.Mock
import Test.QuickCheck
import Data.Text
import Data.Time.Clock
import Data.Time.Calendar
import qualified Network.Wai.Handler.Warp as Warp

-- ------------------------------------
-- Arbitrary instances for domain types
-- ------------------------------------


instance Arbitrary Reminder where
  arbitrary =
    Reminder <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary

instance Arbitrary Text where
  arbitrary =
    pack <$> listOf (elements ['a'..'z'])

instance Arbitrary UTCTime where
  arbitrary =
        UTCTime
    <$> (ModifiedJulianDay <$> choose (50000, 100000)) -- 1995 - 2132
    <*> (secondsToDiffTime <$> choose (0, 86400)) -- seconds

instance Arbitrary Repeat where
  arbitrary = elements [Daily, Weekly, Monthly, Yearly, NoRepeat]

instance Arbitrary Priority where
  arbitrary = elements [Low, Medium, High, NoPriority]

instance Arbitrary a => Arbitrary (WithId a) where
  arbitrary =
        WithId
    <$> choose (1, 100000)
    <*> arbitrary

instance Arbitrary a => Arbitrary (WithValues a) where
  arbitrary = WithValues <$> arbitrary


-- ------------------------------------
-- Mock server
-- ------------------------------------


api :: Proxy RemindersAPI
api = Proxy

server :: Server RemindersAPI
server = mock api Proxy

app :: Application
app = serve api server

run :: Int -> IO ()
run port = Warp.run port app
