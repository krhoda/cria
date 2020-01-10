{-# LANGUAGE DeriveGeneric #-}

module Calendar where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data Calendar = Calendar {
    close :: Text,
    date :: Text,
    open :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Calendar
