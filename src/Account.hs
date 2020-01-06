{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Account
  (
    Account,
    parseCash,
    parsePortfolioValue,
    parseLongMarketValue,
    parseEquity,
  ) where

import Data.Aeson
import Data.Text (Text, pack, unpack)

import GHC.Generics

data Account = Account {
    id :: Text,
    account_number :: Text,
    status :: Text,
    currency :: Text,
    cash :: Text, -- Number. Float
    portfolio_value :: Text, -- Number. Float
    pattern_day_trader :: Bool,
    trade_suspended_by_user :: Bool,
    trading_blocked :: Bool,
    transfers_blocked :: Bool,
    account_blocked :: Bool,
    created_at :: Text, -- Timestamp.
    shorting_enabled :: Bool,
    long_market_value :: Text, -- Number. Float
    short_market_value :: Text, -- Number. Float
    equity :: Text, -- Number. Float
    last_equity :: Text, -- Number. Float
    multiplier :: Text, -- Number. Float
    buying_power :: Text,
    initial_margin :: Text, -- Number. Float
    maintenance_margin :: Text, -- Number. Float
    sma :: Text, -- Number. Float
    daytrade_count :: Int,
    last_maintenance_margin :: Text, -- Number. Float
    daytrading_buying_power :: Text, -- Number. Float
    regt_buying_power :: Text -- Number. Float
    } deriving (Show, Eq, Generic)

instance FromJSON Account

parseAlpacaDbl :: (Account -> Text) -> Account -> Double
parseAlpacaDbl x y = read (unpack (x y)) :: Double

parseCash :: Account -> Double
parseCash = parseAlpacaDbl cash

parsePortfolioValue :: Account -> Double
parsePortfolioValue = parseAlpacaDbl portfolio_value

parseLongMarketValue :: Account -> Double
parseLongMarketValue = parseAlpacaDbl long_market_value

parseShortMarketValue :: Account -> Double
parseShortMarketValue = parseAlpacaDbl short_market_value

parseEquity :: Account -> Double
parseEquity = parseAlpacaDbl equity

parseLastEquity :: Account -> Double
parseLastEquity = parseAlpacaDbl last_equity

parseMultiplier :: Account -> Double
parseMultiplier = parseAlpacaDbl multiplier

parseInitialMargin :: Account -> Double
parseInitialMargin = parseAlpacaDbl initial_margin

parseMaintanceMargin :: Account -> Double
parseMaintanceMargin = parseAlpacaDbl maintenance_margin

parseSMA :: Account -> Double
parseSMA = parseAlpacaDbl sma

parseLastMaintenanceMargin :: Account -> Double
parseLastMaintenanceMargin = parseAlpacaDbl last_maintenance_margin

parseDaytradingBuyingPower :: Account -> Double
parseDaytradingBuyingPower = parseAlpacaDbl daytrading_buying_power

parseRegtBuyingPower :: Account -> Double
parseRegtBuyingPower = parseAlpacaDbl regt_buying_power
