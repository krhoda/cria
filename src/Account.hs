{-# LANGUAGE DeriveGeneric #-}

module Account where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

import Alparseable

data Account = Account {
    id :: Text, -- TODO: Create Custom Parser.
    account_number :: Alparseable,
    status :: Text,
    currency :: Text,
    cash :: Alparseable, -- Number. Float
    portfolio_value :: Alparseable, -- Number. Float
    pattern_day_trader :: Bool,
    trade_suspended_by_user :: Bool,
    trading_blocked :: Bool,
    transfers_blocked :: Bool,
    account_blocked :: Bool,
    created_at :: Text, -- Timestamp.
    shorting_enabled :: Bool,
    long_market_value :: Alparseable, -- Number. Float
    short_market_value :: Alparseable, -- Number. Float
    equity :: Alparseable, -- Number. Float
    last_equity :: Alparseable, -- Number. Float
    multiplier :: Alparseable, -- Number. Float
    buying_power :: Alparseable, -- Number. Float
    initial_margin :: Alparseable, -- Number. Float
    maintenance_margin :: Alparseable, -- Number. Float
    sma :: Alparseable, -- Number. Float
    daytrade_count :: Int,
    last_maintenance_margin :: Alparseable, -- Number. Float
    daytrading_buying_power :: Alparseable, -- Number. Float
    regt_buying_power :: Alparseable -- Number. Float
    } deriving (Show, Eq, Generic)

instance FromJSON Account
