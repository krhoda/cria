{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Account
import Cria
import Alparseable

-- TODO: REMOVE AND ACCEPT THEM PASSED IN.
import System.Environment

import Servant.Client

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)

someFunc :: IO ()
someFunc = do
  mgmt  <- newManager tlsManagerSettings
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  res <- runClientM (getAccountStatus key secret) (mkClientEnv mgmt paperAlpacaBase)
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right acct -> do
          print (alparse (status acct))
          print (alparse (buying_power acct))
          print (alparse (cash acct))

getAccountStatus :: String -> String -> ClientM Account
getAccountStatus key secret = do
  acct <- account (Just key) (Just secret)
  return acct
