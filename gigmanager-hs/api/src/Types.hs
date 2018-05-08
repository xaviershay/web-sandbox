{-# LANGUAGE DeriveGeneric       #-}

module Types where

import Crypto.JWT (JWKSet)
import Data.Aeson (ToJSON)
import Control.Concurrent (MVar)
import GHC.Generics (Generic)
import Data.Time (UTCTime, Day)
import qualified Data.Text as T

data Account = Account

data AppConfig = AppConfig
  { cnfJwk :: MVar JWKSet
  }

newtype QuoteId = QuoteId Int deriving (Show, Eq, Generic)
newtype QuoteDayId = QuoteDayId Int deriving (Show, Eq, Generic)

data QuoteDay = QuoteDay {
  quoteDayId        :: QuoteDayId,
  quoteDayOccursOn  :: Day,
  quoteDayNotes     :: T.Text,
  quoteDayCreatedAt :: UTCTime,
  quoteDayUpdatedAt :: UTCTime
} deriving (Show, Generic)

data Quote = Quote {
  quoteId          :: QuoteId,
  quoteDescription :: T.Text,
  quoteDays        :: [QuoteDay],
  quoteCreatedAt  :: UTCTime,
  quoteUpdatedAt  :: UTCTime
} deriving (Show, Generic)

instance ToJSON QuoteId
instance ToJSON QuoteDayId
instance ToJSON QuoteDay
instance ToJSON Quote
