{-# LANGUAGE DeriveGeneric #-}

module Record.RequestBodies where

import Data.Aeson
import Data.Text (Text, unpack)

import GHC.Generics (Generic)

data WatchlistPost = WatchlistPost {
  name :: Text,
  symbols :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON WatchlistPost
instance ToJSON WatchlistPost

data WatchlistSymbolPost = WatchlistSymbolPost {
  symbol :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON WatchlistSymbolPost
instance ToJSON WatchlistSymbolPost
