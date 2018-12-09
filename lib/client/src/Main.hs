module Main where

import           Client
import qualified Data.Time as T
import           Reminders
import           Server

main :: IO ()
main = do
  let url = "http://localhost:8080"
  print =<< listReminders url
  print =<< createReminder url newReminder
  print =<< removeReminder url 1001
  print =<< updateReminder url 1001 newReminder

  where
    newReminder =
      Reminder
        { title    = "Wake up"
        , onADay   = T.UTCTime (T.fromGregorian 2019 11 14) (T.secondsToDiffTime 540)
        , repeat   = NoRepeat
        , priority = High
        , note     = Nothing
        }
