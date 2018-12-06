module Main where

import           Servant.Client
import           Server
import           Reminders
import           Servant
import qualified Data.Time           as T
import qualified Network.HTTP.Client as HTTP

getReminders :: ClientM [WithId Reminder]
postReminder :: Reminder -> ClientM [WithId Reminder]
deleteReminder :: Integer -> ClientM NoContent
putReminder :: Integer -> Reminder -> ClientM Reminder
getReminders :<|> postReminder :<|> deleteReminder :<|> putReminder
  = client api

main :: IO ()
main = do
  baseUrl <- parseBaseUrl "http://localhost:8080"
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let env = mkClientEnv manager baseUrl

  print =<< runClientM getReminders env
  print =<< runClientM (postReminder newReminder) env
  print =<< runClientM (deleteReminder 1001) env
  print =<< runClientM (putReminder 1001 newReminder) env

  where
    newReminder =
      Reminder
       { title    = "Wake up"
       , onADay   = T.UTCTime (T.fromGregorian 2019 11 14) (T.secondsToDiffTime 540)
       , repeat   = NoRepeat
       , priority = High
       , note     = Nothing
       }
