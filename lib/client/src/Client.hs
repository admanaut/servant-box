module Client
  ( listReminders
  , createReminder
  , removeReminder
  , updateReminder
  ) where

import Network.HTTP.Client
import Reminders
import Servant
import Servant.Client
import Server

-- | Use Server.client to generate client functions.
getReminders :: ClientM [WithId Reminder]
postReminder :: Reminder -> ClientM [WithId Reminder]
deleteReminder :: Integer -> ClientM [WithId Reminder]
putReminder :: Integer -> Reminder -> ClientM [WithId Reminder]
getReminders :<|> postReminder :<|> deleteReminder :<|> putReminder
  = client remindersApi

listReminders :: String -> IO (Either ServantError [WithId Reminder])
listReminders url
  = mkEnv url >>= runClientM getReminders

createReminder
  :: String
  -> Reminder
  -> IO (Either ServantError [WithId Reminder])

createReminder url r
  = mkEnv url >>= runClientM (postReminder r)

removeReminder
  :: String
  -> Integer
  -> IO (Either ServantError [WithId Reminder])

removeReminder url rid
  = mkEnv url >>= runClientM (deleteReminder rid)

updateReminder
  :: String
  -> Integer
  -> Reminder
  -> IO (Either ServantError [WithId Reminder])

updateReminder url rid r
  = mkEnv url >>= runClientM (putReminder rid r)

mkEnv :: String -> IO ClientEnv
mkEnv url
  = do
      base <- parseBaseUrl url
      mnager <- newManager defaultManagerSettings
      pure (mkClientEnv mnager base)
