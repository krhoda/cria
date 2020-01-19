{-# LANGUAGE DeriveGeneric #-}

module Record.Calendar where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data Calendar = Calendar {
    close :: Text, -- %Y-%m-%d
    date :: Text, -- HH:MM
    open :: Text-- HH:MM
  } deriving (Show, Eq, Generic)

instance FromJSON Calendar
