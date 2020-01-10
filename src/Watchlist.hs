{-# LANGUAGE DeriveGeneric #-}

module Watchlist where

import Asset

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data Watchlist = Watchlist {
       account_id :: Text,
       assets :: Maybe [Asset],
       created_at :: Text,
       id :: Text,
       name :: Text,
       updated_at :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON Watchlist where
      parseJSON = genericParseJSON defaultOptions { omitNothingFields  = True }
