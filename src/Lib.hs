{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Account

-- TODO: REMOVE AND ACCEPT THEM PASSED IN.
import System.Environment

import Data.Aeson
import Data.Proxy

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

type Alpaca = "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account

cira :: Proxy Alpaca
cira = Proxy

account = client cira
paperAlpacaBase = BaseUrl Https "paper-api.alpaca.markets" 443 "/v2"

someFunc :: IO ()
someFunc = do
  mgmt  <- newManager tlsManagerSettings
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  res <- runClientM (getAccountStatus key secret) (mkClientEnv mgmt paperAlpacaBase)
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right acct -> do
          print (parseCash acct)
          print (parseEquity acct)

getAccountStatus :: String -> String -> ClientM Account
getAccountStatus key secret = do
  acct <- account (Just key) (Just secret)
  return acct
