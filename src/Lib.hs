{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

-- TODO: REMOVE AND ACCEPT THEM PASSED IN.
import System.Environment

import Data.Aeson
import Data.Proxy
import Data.Text (Text, pack, unpack)

import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

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

parsedCash :: Account -> Double
parsedCash = parseAlpacaDbl cash

parsedPortfolioValue :: Account -> Double
parsedPortfolioValue = parseAlpacaDbl portfolio_value

parsedLongMarketValue :: Account -> Double
parsedLongMarketValue = parseAlpacaDbl long_market_value

parsedShortMarketValue :: Account -> Double
parsedShortMarketValue = parseAlpacaDbl short_market_value

parsedEquity :: Account -> Double
parsedEquity = parseAlpacaDbl equity

parsedLastEquity :: Account -> Double
parsedLastEquity = parseAlpacaDbl last_equity

parsedMultiplier :: Account -> Double
parsedMultiplier = parseAlpacaDbl multiplier

parsedInitialMargin :: Account -> Double
parsedInitialMargin = parseAlpacaDbl initial_margin

parsedMaintanceMargin :: Account -> Double
parsedMaintanceMargin = parseAlpacaDbl maintenance_margin

parsedSMA :: Account -> Double
parsedSMA = parseAlpacaDbl sma

parsedLastMaintenanceMargin :: Account -> Double
parsedLastMaintenanceMargin = parseAlpacaDbl last_maintenance_margin

parsedDaytradingBuyingPower :: Account -> Double
parsedDaytradingBuyingPower = parseAlpacaDbl daytrading_buying_power

parsedRegtBuyingPower :: Account -> Double
parsedRegtBuyingPower = parseAlpacaDbl regt_buying_power

type Alpaca = "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account

cira :: Proxy Alpaca
cira = Proxy

account = client cira
paperAlpacaBase = BaseUrl Https "paper-api.alpaca.markets" 443 "/v2"

someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
someFunc = do
  -- mgmt  <- newManager defaultManagerSettings
  mgmt  <- newManager tlsManagerSettings
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  res <- runClientM (getAccountStatus key secret) (mkClientEnv mgmt paperAlpacaBase)
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (acct) -> do
          print (parsedCash acct)
          print (parsedEquity acct)


getAccountStatus :: String -> String -> ClientM (Account)
getAccountStatus key secret = do
  acct <- account (Just key) (Just secret)
  return (acct)
