module Main where

import Servant.Client
import Server
import Reminders
import Servant

getReminders :: ClientM [WithId Reminder]
postReminder :: Reminder -> ClientM [WithId Reminder]
deleteReminder :: Integer -> ClientM NoContent
putReminder :: Integer -> Reminder -> ClientM Reminder
getReminders :<|> postReminder :<|> deleteReminder :<|> putReminder
  = client api

main :: IO ()
main = pure ()
