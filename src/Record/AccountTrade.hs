{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AccountTrade where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (fieldLabelModifier))

import Data.Text (Text)

import GHC.Generics (Generic)

data AccountTrade = AccountTrade {
  activity_type :: Text
  ,cum_qty :: Text
  ,account_trade_id :: Text
  ,leaves_qty :: Text
  ,price :: Text
  ,qty :: Text
  ,side :: Text
  ,symbol :: Text
  ,transaction_time :: Text
  ,account_trade_type :: Text
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \x ->
  case x of
  "account_trade_id" -> "id"
  "account_trade_type" -> "type"
  _ -> x} ''AccountTrade)
