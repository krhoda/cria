{-# LANGUAGE DeriveGeneric #-}

module Record.Clock where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data Clock = Clock {
    next_close :: Text,
    next_open :: Text,
    timestamp :: Text,
    is_open :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Clock
