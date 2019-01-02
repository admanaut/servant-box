{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Reminders where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Calendar as T
import Data.Time.Clock as T
import GHC.Generics
import Safe (headMay)
import Servant

import Prelude hiding (repeat)

data Repeat
  = Daily
  | Weekly
  | Monthly
  | Yearly
  | NoRepeat
  deriving (Eq, Show, Generic)

data Priority
  = Low
  | Medium
  | High
  | NoPriority
  deriving (Eq, Show, Generic)

data Reminder
  = Reminder
      { title    :: !Text
      , onADay   :: !T.UTCTime
      , repeat   :: !Repeat
      , priority :: !Priority
      , note     :: !(Maybe Text)
      }
  deriving (Eq, Show, Generic)

data WithId a
  = WithId
      { id    :: Integer
      , value :: a
      }
  deriving (Eq, Show, Generic)

-- | Wrapper for API responses, so that all responses
-- encode to JSON Object, which apparently is best practice.
newtype WithValues a = WithValues { values :: a } deriving (Show, Generic)

-- | Just a type synonym for API response.
type AllReminders = WithValues [WithId Reminder]

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

type RemindersAPI
  =    "reminders" :> Get '[JSON] AllReminders
  :<|> "reminder"  :> ReqBody '[JSON] Reminder :> Post '[JSON] AllReminders
  :<|> "reminder"  :> Capture "id" Integer :> Delete '[JSON] AllReminders
  :<|> "reminder"  :> Capture "id" Integer :> ReqBody '[JSON] Reminder :> Put '[JSON] AllReminders

remindersApi :: Proxy RemindersAPI
remindersApi = Proxy

reminders :: Server RemindersAPI
reminders = getReminders
  :<|> postReminder
  :<|> deleteReminder
  :<|> putReminder

  where
    withValues = pure . WithValues
    getReminders :: Handler AllReminders
    getReminders = withValues defaultReminders

    postReminder :: Reminder -> Handler AllReminders
    postReminder r
      = if repeat r == Yearly -- testing only
        then throwError err400
        else withValues (r' : defaultReminders)
      where
        r' = WithId nextId r
        nextId =
          (+ 1)
          $ maximum
          $ (\(WithId id _) -> id) <$> defaultReminders

    deleteReminder :: Integer -> Handler AllReminders
    deleteReminder id
      = case splitRemindersAt id of
         (_, [])      -> throwError err404
         (xs, (_:ys)) -> withValues (xs <> ys)

    findReminder :: Integer -> Maybe (WithId Reminder)
    findReminder id
      = headMay $
        dropWhile (\(WithId id' _) -> id' /= id) defaultReminders

    putReminder :: Integer -> Reminder -> Handler AllReminders
    putReminder id r
      = case splitRemindersAt id of
          (_, []) -> throwError err404
          (xs, (r':ys)) -> withValues (xs <> [updateReminder r'] <> ys)
      where
        updateReminder (WithId id r')
          = WithId id $
              r' { title    = title r
                 , onADay   = onADay r
                 , repeat   = repeat r
                 , priority = priority r
                 , note     = note r
                 }

    splitRemindersAt id
      = let idEq = \(WithId id' _) -> id' /= id
        in ( takeWhile idEq defaultReminders
           , dropWhile idEq defaultReminders
           )

-- }}} Api

deriveJSON defaultOptions ''Repeat
deriveJSON defaultOptions ''Priority
deriveJSON defaultOptions ''Reminder
deriveJSON defaultOptions ''WithId
deriveJSON defaultOptions ''WithValues
