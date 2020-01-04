{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where

-- TODO: REMOVE AND ACCEPT THEM PASSED IN.
import System.Environment

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

newtype Account = Account { status :: String }
  deriving (Show, Generic)

instance FromJSON Account

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
        Right (acct) -> print acct

getAccountStatus :: String -> String -> ClientM (Account)
getAccountStatus key secret = do
  acct <- account (Just key) (Just secret)
  return (acct)
