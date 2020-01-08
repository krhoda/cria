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

someFunc :: IO ()
someFunc = do
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  cli <- return (newCriaClient (key, secret, False))
  res <- signAndRun cli account
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right acct -> do
          print (alparse (status acct))
          print (alparse (buying_power acct))
          print (alparse (cash acct))

getAccountStatus :: String -> String -> ClientM Account
getAccountStatus key secret = account (Just key) (Just secret)
