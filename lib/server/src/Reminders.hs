{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Reminders where

import Data.Aeson.TH
import Data.Aeson
import Data.Time.Clock    as T
import Data.Time.Calendar as T
import Data.Text          (Text)
import Servant

import Prelude     hiding (repeat)

data Repeat
  = Daily
  | Weekly
  | Monthly
  | Yearly
  | NoRepeat
  deriving (Eq, Show)

data Priority
  = Low
  | Medium
  | High
  | NoPriority
  deriving (Eq, Show)

data Reminder
  = Reminder
      { title    :: !Text
      , onADay   :: !T.UTCTime
      , repeat   :: !Repeat
      , priority :: !Priority
      , note     :: !(Maybe Text)
      }
  deriving (Eq, Show)

data WithId a
  = WithId
      { id    :: Integer
      , value :: a
      }
  deriving (Eq, Show)

defaultReminders :: [WithId Reminder]
defaultReminders =
  [  WithId 1001 $
       Reminder
        { title    = "Buy bread"
        , onADay   = T.UTCTime (T.fromGregorian 2019 10 14) (T.secondsToDiffTime 540)
        , repeat   = NoRepeat
        , priority = Medium
        , note     = Nothing
        }

  , WithId 1002 $
      Reminder
        { title    = "Feed the cat"
        , onADay   = T.UTCTime (T.fromGregorian 2018 10 25) (T.secondsToDiffTime 540)
        , repeat   = Daily
        , priority = High
        , note     = Just "food is in the storage room"
        }
  ]

-- {{{ API

type API
  =    "reminders" :> Get '[JSON] [WithId Reminder]
  :<|> "reminder"  :> ReqBody '[JSON] Reminder :> Post '[JSON] [WithId Reminder]
  :<|> "reminder"  :> Capture "id" Integer :> DeleteNoContent '[JSON] NoContent

handler :: Server API
handler = getReminders
  :<|> postReminder
  :<|> deleteReminder

  where
    getReminders :: Handler [WithId Reminder]
    getReminders = pure defaultReminders

    postReminder :: Reminder -> Handler [WithId Reminder]
    postReminder r = pure (r' : defaultReminders)
      where
        r' = WithId nextId r
        nextId =
          (+ 1)
          $ maximum
          $ (\(WithId id _) -> id) <$> defaultReminders

    deleteReminder :: Integer -> Handler NoContent
    deleteReminder id = pure NoContent -- TODO

-- }}} Api

deriveJSON defaultOptions ''Repeat
deriveJSON defaultOptions ''Priority
deriveJSON defaultOptions ''Reminder
deriveJSON defaultOptions ''WithId
