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
getReminders :: ClientM AllReminders
postReminder :: Reminder -> ClientM AllReminders
deleteReminder :: Integer -> ClientM AllReminders
putReminder :: Integer -> Reminder -> ClientM AllReminders
getReminders :<|> postReminder :<|> deleteReminder :<|> putReminder
  = client remindersApi

listReminders :: String -> IO (Either ServantError AllReminders)
listReminders url
  = mkEnv url >>= runClientM getReminders

createReminder
  :: String
  -> Reminder
  -> IO (Either ServantError AllReminders)

createReminder url r
  = mkEnv url >>= runClientM (postReminder r)

removeReminder
  :: String
  -> Integer
  -> IO (Either ServantError AllReminders)

removeReminder url rid
  = mkEnv url >>= runClientM (deleteReminder rid)

updateReminder
  :: String
  -> Integer
  -> Reminder
  -> IO (Either ServantError AllReminders)

updateReminder url rid r
  = mkEnv url >>= runClientM (putReminder rid r)

mkEnv :: String -> IO ClientEnv
mkEnv url
  = do
      base <- parseBaseUrl url
      mnager <- newManager defaultManagerSettings
      pure (mkClientEnv mnager base)
