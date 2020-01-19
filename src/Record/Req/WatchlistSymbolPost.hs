{-# LANGUAGE DeriveGeneric #-}

module Record.Req.WatchlistSymbolPost where

import Data.Aeson
import Data.Text (Text, unpack)

import GHC.Generics (Generic)

data WatchlistSymbolPost = WatchlistSymbolPost {
  symbol :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON WatchlistSymbolPost
instance ToJSON WatchlistSymbolPost
